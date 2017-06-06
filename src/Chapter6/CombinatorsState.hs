{-# LANGUAGE TypeSynonymInstances, LiberalTypeSynonyms #-}

module Chapter6.CombinatorsState where

import Chapter6.Vector

import Data.Default
import Data.List
import qualified Data.Map as M

type State s a = s -> (a, s)

--thenDo :: (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)
--thenDo f g s = let (resultOfF, stateAfterF) = f s
--                in g resultOfF stateAfterF

thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g = uncurry g . f
-- We know function f returns a tuple (a,s). Function g takes two
-- arguments, a and s. If we uncurry g, then it will take one
-- argument, (a,s). Since uncurry g can take the return value of
-- function f, we can compose them.
