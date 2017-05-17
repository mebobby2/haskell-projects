module Chapter3.ParamPoly where

--GovOrg {clientId = 1, clientName = "MOM"}
data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String}
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName :: String }
              deriving Show

data Triple a b c = Triple a b c
-- :t Triple 1 "a" 0.0
--  Triple 1 "a" 0.0 :: (Num a, Fractional c) => Triple a [Char] c

data SamePair a = SamePair a a
-- :t SamePair 1 2
--  SamePair 1 2 :: Num a => SamePair a
-- :t SamePair 1 "sdf"
--  Gives error