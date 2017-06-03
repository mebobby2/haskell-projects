{-# LANGUAGE TemplateHaskell #-}

module Chapter6.Lens2 where

import Control.Lens

data Client i = GovOrg     { _identifier :: i, _name :: String }
              | Company    { _identifier :: i, _name :: String, _person :: Person, _duty :: String }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show

data Person = Person { _firstName :: String, _lastName :: String }
              deriving Show

makeLenses ''Client
makeLenses ''Person

fullName :: Simple Lens Person String
fullName = lens (\(Person f l)  -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                      f:l:_ -> Person f l
                                      _     -> error "Incorrect name")

--let p = Person "John" "Smith" in (view firstName p, p^.lastName)

--let client = Individual 3 (Person "John" "Smith")
--client^.person.fullName
--view (person . lastName) client

--set identifier 10 client
--person.lastName .~ "Kox" $ client
--client & person.fullName .~ "Marianne Kox"

--client & identifier +~ 2
--client & over identifier (+2)
--client & person.fullName %~ (map toUpper)
--over is a general function, and %~ is its infix form

--("a","b") & set _1 "c"

--"abc"^?_head
--"abc"^?!_tail
--"abc" & (_head .~ 'd')
--"abc" & (_tail %~ map toUpper)

--"abc"^?_init
--"abc" & (_last %~ toUpper)

--let people = [Person "Jack" "Smith", Person "Marianne" "Branson"]
--people & traversed.firstName %~ map toUpper
