module Chapter2.DefaultValues where

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut  = NoTimeOut | TimeOut Integer

data Connection = Connection
                deriving Show

connect :: String -> ConnType -> Integer -> UseProxy -> Bool -> Bool -> TimeOut -> Connection
connect _ _ _ _ _ _ _ = Connection

connectUrl :: String -> Connection
connectUrl u = connect u TCP 0 NoProxy False False NoTimeOut



data ConnOptions = ConnOptions { connType :: ConnType
                               , connSpeed :: Integer
                               , connProxy :: UseProxy
                               , connCaching :: Bool
                               , connKeppAlive :: Bool
                               , connTimeOut :: TimeOut
                               }

connect' :: String -> ConnOptions -> Connection
connect' _ _ = Connection

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut


--connect' "http://apress.com" connDefault
--connect' "http://apress.com" connDefault { connType = UDP }