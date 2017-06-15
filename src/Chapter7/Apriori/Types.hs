module Chapter7.APriori.Types where

import Data.Set (Set)
import qualified Data.Set as S

-- Clients
data Client = GovOrg     { clientName :: String }
            | Company    { clientName :: String, person:: Person, duty :: String }
            | Individual { person :: Person }
            deriving (Show, Eq, Ord)
data ClientKind = KindGovOrg | KindCompany | KindIndividual
                deriving (Show, Eq, Ord)
data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
            deriving (Show, Eq, Ord)
data Gender = Male | Female | UnknownGender
            deriving (Show, Eq, Ord)

-- Products
data Product = Product { productId :: Integer, productType :: ProductType }
             deriving (Show, Eq, Ord)
data ProductType = TimeMachine | TravelGuide | Tool | Trip
                 deriving (Show, Eq, Ord)



data Purchase = Purchase { client :: Client, products :: [Product] }
              deriving (Show, Eq, Ord)
data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty String
                  | InfoClientGender Gender
                  | InfoPurchasedProduct Integer
                  | InfoPurchasedProductType ProductType
                  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)