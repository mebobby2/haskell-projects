{-# LANGUAGE TemplateHaskell #-}

module Chapter6.StateLenses where

import Chapter6.Lens2

import Control.Lens
import Control.Monad.State
import Data.Char

data ExampleState = ExampleState { _increment :: Int, _clients :: [Client Int] }
                  deriving Show

makeLenses ''ExampleState

zoomExample :: State ExampleState ()
zoomExample = do n <- use increment
                 zoom (clients.traversed) $ do
                    identifier += n
                    person.fullName %= map toUpper

--Individual 4 (Person "John" "Smith")
--Gives this error:
--Defaulting the following constraints to type ‘Integer’
--This is because the data type Client is defined to be polymorphic, so the compiler
--doesn't know if 4 is supposed to be an Int or a Integer.
--To fix, use: Individual (4 :: Int) (Person "John" "Smith")
--If you assign it to a variable, like so: client1 = Individual 4 (Person "John" "Smith")
--It works, but as soon as u try to print it out, like so: client1, it crashes with the same error

:{
  let client1 = Individual 4 (Person "John" "Smith")
      client2 = Individual 3 (Person "Albert" "Einsten")
   in execState zoomExample (ExampleState 2 [client1, client2])
:}
