module Chapter2.DataTypes where

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data Manufacturer = Manufacturer String deriving Show
data Model = Model Integer deriving Show
data TimeMachine = TimeMachine String Manufacturer Model Bool Double
                 deriving Show