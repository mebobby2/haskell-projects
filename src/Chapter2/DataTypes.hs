module Chapter2.DataTypes where

-- c = Company "Comware" 3 (Person "Bobby" "Lei" Male) "CEO"
-- g = GovOrg "Nasa"
-- i = Individual (Person "Bobby" "Lei" Male) True

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

--clientName :: Client -> String
--clientName client  = case client of
--                      GovOrg name -> name
--                      Company name id person resp -> name
--                      Individual person ads ->
--                        case person of Person fName lName gender -> fName ++ " " ++ lName

--clientName :: Client -> String
--clientName client = case client of
--                      GovOrg name -> name
--                      Company name id person resp -> name
--                      Individual (Person fName lName _) _ -> fName ++ " " ++ lName

clientName :: Client -> String
clientName (GovOrg name) = name
clientName (Company name _ _ _) = name
clientName (Individual (Person fName lName _) _) = fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client = case client of
                      Company name _ _ _ -> Just name
                      _                  -> Nothing

--let Just name = companyName client