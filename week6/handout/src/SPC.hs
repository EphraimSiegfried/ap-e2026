module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
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
  | MsgJob Job (ReplyChan JobId)

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

startSPC :: IO SPC
startSPC = do
  server <- spawn $ \input -> runSPCM (SPCState 0 []) (forever $ handleMsg input)
  pure $ SPC server

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ receive c
  case msg of
    MsgPing rsvp -> do
      state <- get
      io $ reply rsvp $ spcPingCounter state
      put $ state {spcPingCounter = succ $ spcPingCounter state}
    MsgJob job rsvp ->
      let jid = JobId 1
       in do
            state <- get
            io $ reply rsvp $ jid
            put $ state {spcPendingJobs = (jid, job) : (spcPendingJobs state)}

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcPingCounter :: Int,
    spcPendingJobs :: [(JobId, Job)]
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
jobAdd (SPC srv) job = requestReply srv $ MsgJob job
