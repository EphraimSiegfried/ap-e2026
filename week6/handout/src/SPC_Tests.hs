module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent as CC
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "ping" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 0,
        testCase "state change" $ do
          spc <- startSPC
          _ <- pingSPC spc
          _ <- pingSPC spc
          x <- pingSPC spc
          x @?= 2,
        testCase "Add job" $
          let job = Job (putStrLn "hi") 20
           in do
                spc <- startSPC
                x <- jobAdd spc job
                x @?= (JobId 0),
        testCase "Query job status" $
          let job = Job (putStrLn "hi") 20
           in do
                spc <- startSPC
                jid <- jobAdd spc job
                status <- jobStatus spc jid
                status @?= (Just JobPending)
                status2 <- jobStatus spc (JobId 2)
                status2 @?= Nothing,
        testCase "Cancel Job" $
          let job = Job (putStrLn "hi") 20
           in do
                spc <- startSPC
                jid <- jobAdd spc job
                _ <- jobCancel spc jid
                status <- jobStatus spc jid
                status @?= (Just (JobDone DoneCancelled)),
        testCase "Wait Job" $
          let job = Job (putStrLn "hi") 20
           in do
                spc <- startSPC
                jid <- jobAdd spc job
                _ <- jobCancel spc jid
                status <- jobWait spc jid
                status @?= (Just DoneCancelled)
      ]
