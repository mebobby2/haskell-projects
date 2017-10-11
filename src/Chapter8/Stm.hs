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


main3 :: IO ()
main3 = do v <- newMVar 10000
           s <- newMVar [("a", 7)]
           forkDelay 5 $ updateMoneyAndStock "a" 1000 v s
           forkDelay 5 $ printMoneyAndStock v s
           _ <- getLine -- to wait for completion
           return ()

updateMoneyAndStock :: Eq a => a -> Integer -> MVar Integer -> MVar [(a, Integer)] -> IO ()
updateMoneyAndStock product price money stock =
  do s <- takeMVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- takeMVar money
               let newS = map (\(k,v) -> if k == product then (k,v-1) else (k,v)) s
               putMVar money (m + price) >> putMVar stock newS
       else putMVar stock s

printMoneyAndStock :: Show a => MVar Integer -> MVar [(a, Integer)] -> IO ()
printMoneyAndStock money stock = do m <- readMVar money
                                    s <- readMVar stock
                                    putStrLn $ show m ++ "\n" ++ show s

updateMoneyAndStockStm :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
updateMoneyAndStockStm product price money stock =
  do s <- readTVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- readTVar money
               let newS = map (\(k,v) -> if k == product then (k,v-1) else (k,v)) s
               writeTVar money (m+price) >> writeTVar stock newS
       else return ()


main4 :: IO ()
main4 = do v <- newTVarIO 10000
           s <- newTVarIO [("a", 7)]
           forkDelay 5 $ atomically $ updateMoneyAndStockStm "a" 1000 v s
           _ <- getLine
           return ()


payByCard :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
payByCard product price money stock =
  do working <- isCardSystemWorking
     if not working
     then retry
     else updateMoneyAndStockStm product price money stock

isCardSystemWorking :: STM Bool
isCardSystemWorking = undefined

pay :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
pay product price money stock = payByCard product price money stock `orElse`
                                payByCash product price money stock

payByCash :: Eq a => a -> Integer -> TVar Integer -> TVar [(a,Integer)] -> STM ()
payByCash = undefined

main5 = do q <- newTQueueIO
           forkIO $ backend q -- create one backend
           replicateM_ 10 $ forkIO (frontend q) -- create 10 frontends
           _ <- getLine
           return ()

backend :: TQueue (String, Integer) -> IO ()
backend q = do
  m <- newTVarIO 10000
  s <- newTVarIO [("a", 7)]
  forever $ atomically $ do (product, price) <- readTQueue q
                            pay product price m s

frontend :: TQueue (String, Integer) -> IO ()
frontend q = do (product,price) <- askClient
                atomically $ writeTQueue q (product, price)

askClient :: IO (String, Integer)
askClient = undefined
