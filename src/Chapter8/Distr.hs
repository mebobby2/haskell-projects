{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, DoAndIfThenElse #-}

module Main where

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Control.Distributed.Process
import System.Environment
import System.Random
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

data GalaxyMessage = LookForGalaxy ProcessId | GalaxyFound string
                   deriving (Typeable, Generic)
instance Binary GalaxyMessage

data WormHoleMessage = LostInWormHole
                     deriving (Typeable, Generic)
instance Binary WormHoleMessage

traveller :: Process ()
traveller = do LookForGalaxy master <- expect
               b <- liftIO % randomIO -- generate random Boolean
               if b
               then send master (GalaxyFound "Andromeda")
               else send master LostInWormHole

master :: [NodeId] -> Process ()
master nodes =
  do myPid <- getSelfPid
     mapM_ (\node -> do pid <- spawn node $(mkStaticClosure 'traveller)
                        send pid (LookForGalaxy myPid))
           nodes
      forever $ do receiveWait
                    [ match $ \(GalaxyFound g) -> say $ "Found galaxy :" ++ g
                    , match $ \LostInWormHole  -> say "Lost in wormhole"
                    ]

main :: IO ()
main = do args <- getArgs
          case args of
            ["master", host, port] -> do
              backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
              startMaster backend master
            ["traveller", host, port] -> do
                backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
                startSlave backend
            _ -> do putStrLn "Unknown parameters"
