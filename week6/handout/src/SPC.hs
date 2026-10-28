module SPC
  ( -- * SPC startup
    SPC,
    Job (..),
    JobId (..),
    JobStatus (..),
    JobDoneReason (..),
    jobStatus,
    startSPC,
    pingSPC,
    jobAdd,
    jobCancel,
    jobWait,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  deriving (Eq, Ord, Show)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg
  = MsgPing (ReplyChan Int)
  | MsgJobAdd Job (ReplyChan JobId)
  | MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | MsgJobCancel JobId (ReplyChan ())
  | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

startSPC :: IO SPC
startSPC = do
  server <- spawn $ \input -> runSPCM (SPCState 0 [] [] []) (forever $ handleMsg input)
  pure $ SPC server

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ receive c
  case msg of
    MsgPing rsvp -> do
      state <- get
      io $ reply rsvp $ spcPingCounter state
      put $ state {spcPingCounter = succ $ spcPingCounter state}
    MsgJobAdd job rsvp ->
      do
        state <- get
        let jid = spcPingCounter state
        put $ state {spcPingCounter = succ $ jid, spcPendingJobs = (JobId jid, job) : (spcPendingJobs state)}
        io $ reply rsvp $ JobId jid
    MsgJobStatus jid rsvp ->
      do
        state <- get
        let pending = spcPendingJobs state
        let cancelled = spcDoneJobs state
        case lookup jid pending of
          Just _ -> io $ reply rsvp $ (Just JobPending)
          _ -> case lookup jid cancelled of
            Just _ -> io $ reply rsvp $ (Just (JobDone DoneCancelled))
            _ -> io $ reply rsvp $ Nothing
    MsgJobCancel jid rsvp ->
      do
        state <- get
        case lookup jid (spcPendingJobs state) of
          Just _ ->
            let newDoneJobs = (jid, DoneCancelled) : (spcDoneJobs state)
                newPendingJobs = filter (\s -> (fst s) /= jid) (spcPendingJobs state)
             in do
                  put $ state {spcDoneJobs = newDoneJobs, spcPendingJobs = newPendingJobs}
                  forM_ (spcJobWaiters state) $ \from ->
                    if (fst from) == jid
                      then do
                        io $ reply (snd from) $ (Just DoneCancelled)
                        put $ state {spcJobWaiters = filter (\s -> (fst s) /= jid) (spcJobWaiters state)}
                      else pure ()
          _ -> pure ()
        io $ reply rsvp $ ()
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcDoneJobs state of
        Just reason -> do
          io $ reply rsvp $ Just reason
        Nothing ->
          if jobid >= JobId (spcPingCounter state)
            then io $ reply rsvp Nothing
            else put $ state {spcJobWaiters = (jobid, rsvp) : spcJobWaiters state}

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcPingCounter :: Int,
    spcPendingJobs :: [(JobId, Job)],
    spcDoneJobs :: [(JobId, JobDoneReason)],
    spcJobWaiters :: [(JobId, ReplyChan (Maybe JobDoneReason))]
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \s -> do
    (v, s') <- m s
    let SPCM f' = f v
    f' s'

get :: SPCM SPCState
get = SPCM $ \s -> pure (s, s)

put :: SPCState -> SPCM ()
put s = SPCM $ \_ -> pure ((), s)

io :: IO a -> SPCM a
io a = SPCM $ \state -> do
  x <- a
  pure (x, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM s (SPCM g) = do
  (a, _) <- g s
  pure a

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

pingSPC :: SPC -> IO Int
pingSPC (SPC srv) = requestReply srv MsgPing

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC srv) job = requestReply srv $ MsgJobAdd job

-- | Query the job status.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC srv) jid = requestReply srv $ MsgJobStatus jid

jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC srv) jid = requestReply srv $ MsgJobCancel jid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid = requestReply c $ MsgJobWait jobid

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ Just reason
      put $
        state
          { spcJobWaiters = not_waiting_for_job,
            spcDoneJobs = (jobid, reason) : spcJobsDone state,
            spcJobRunning = Nothing,
            spcPendingJobs = removeAssoc jobid $ spcJobsPending state
          }
