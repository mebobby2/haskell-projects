module Chapter8.Stm where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import System.Random

-- When you use takeMVar, you either read the value being held and make the box
-- empty, or block until some element is put in there. Conversely, putMVar
-- either writes a new value if the box is empty, or it waits. Furthermore,
-- those functions guarantee that only one thread will be woken up if it were
-- blocked and that threads will be served in a first-in first-out order, which
-- means that no thread can swallow the events of all the rest.

main1 :: IO ()
main1 = do v <- newMVar 10000
           forkIO $ updateMoney v
           forkIO $ updateMoney v
           forkIO $ updateMoney v
           -- Wait for user input so the main thread does not die.
           -- If main thread dies, all child threads also die.
           _ <- getLine
           return ()

updateMoney :: MVar Integer -> IO ()
updateMoney v = do m <- takeMVar v
                   putStrLn $ "Updating value, which is " ++ show m
                   putMVar v (m + 500) -- suppose a constant price

readMoney :: MVar Integer -> IO ()
readMoney v = do m <- readMVar v
                 putStrLn $ "The current value is " ++ show m

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r * 1000000)

forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

main2 :: IO ()
main2 = do v <- newMVar 10000
           forkDelay 5 $ updateMoney v
           forkDelay 5 $ readMoney v
           _ <- getLine
           return ()
