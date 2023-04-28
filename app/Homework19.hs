{-# LANGUAGE DeriveGeneric #-} -- This pragma is for deriving JSON structures for custom types
{-# LANGUAGE DeriveAnyClass #-} -- This pragma is for deriving JSON structures for custom types
{-# LANGUAGE DuplicateRecordFields #-} -- This pragma is nessecary for question 5, which has duplicate fields in different record types.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson
import GHC.Generics
import Data.Maybe
import Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import Control.Concurrent.Async
import Control.Monad

-- Question 1
-- Add the correct instances that should be derived in the below data types for deriving JSON formats for them.

data Continent
    = Africa | Asia | Europe | NorthAmerica | SouthAmerica | Oceania | Antartica
    deriving (Show, Generic)
instance FromJSON Continent

data Country
    = Country
    { name       :: Maybe String
    , population :: Maybe Int
    , location   :: Maybe Continent
    }
    deriving (Show, Generic)
instance FromJSON Country

-- Question 2
-- Decode these countries to the type Country above.

jsonItaly :: LBS.ByteString
jsonItaly = "{\"location\":\"Europe\",\"name\":\"Italy\",\"population\":55550000}"
-- >>> decode @Country jsonItaly
-- Just (Country {name = Just "Italy", population = Just 55550000, location = Just Europe})

jsonSpain :: LBS.ByteString
jsonSpain = "{\"location\":\"Europe\",\"name\":\"Spain\",\"population\":47350000}"
-- >>> decode @Country jsonSpain
-- Just (Country {name = Just "Spain", population = Just 47350000, location = Just Europe})

jsonAustralia :: LBS.ByteString
jsonAustralia  = "{\"location\":\"Oceania\",\"name\":\"Australia\",\"population\":25690000}"
-- >>> decode @Country jsonAustralia
-- Just (Country {name = Just "Australia", population = Just 25690000, location = Just Oceania})

jsonIndia :: LBS.ByteString
jsonIndia = "{\"location\":\"Asia\",\"name\":\"India\",\"population\":1380000000}"
-- >>> decode @Country jsonIndia
-- Just (Country {name = Just "India", population = Just 1380000000, location = Just Asia})

-- Question 3
-- Add the correct instances that should be derived in the below data type for deriving JSON formats.

data Tree
    = Leaf | LLeaf Tree | RLeaf Tree
    deriving (Show, Generic)
instance FromJSON Tree
instance ToJSON Tree

leaf = Leaf
lleaf = LLeaf leaf
rleaf = RLeaf lleaf

jsonLeaf = encode leaf
-- >>> jsonLeaf
-- "{\"tag\":\"Leaf\"}"

jsonLleaf = encode lleaf
-- >>> jsonLleaf
-- "{\"contents\":{\"tag\":\"Leaf\"},\"tag\":\"LLeaf\"}"

jsonRleaf = encode rleaf
-- >>> jsonRleaf
-- "{\"contents\":{\"contents\":{\"tag\":\"Leaf\"},\"tag\":\"LLeaf\"},\"tag\":\"RLeaf\"}"

{-
>>> decode @Tree "{\"tag\":\"Leaf\"}"
>>> decode @Tree "{\"contents\":{\"tag\":\"Leaf\"},\"tag\":\"LLeaf\"}"
>>> decode @Tree "{\"contents\":{\"contents\":{\"tag\":\"Leaf\"},\"tag\":\"LLeaf\"},\"tag\":\"RLeaf\"}"
Just Leaf
Just (LLeaf Leaf)
Just (RLeaf (LLeaf Leaf))
-}

-- Question 4
-- Encode the below example of a Tree to JSON and try to figure out how it is structured.

exampleLeaf = LLeaf $ RLeaf $ RLeaf $ LLeaf $ Leaf
-- >>> encode exampleLeaf
-- "{\"contents\":{\"contents\":{\"contents\":{\"contents\":{\"tag\":\"Leaf\"},\"tag\":\"LLeaf\"},\"tag\":\"RLeaf\"},\"tag\":\"RLeaf\"},\"tag\":\"LLeaf\"}"

-- Question 4
-- With the below getUser function we can fetch some API that will serve us a user from a list of 10 users.
-- Write a Haskell data type that can convert this JSON representation of a user to a Haskell datatype

data Geo
    = Geo
    { lat :: Maybe String
    , lng :: Maybe String
    }
    deriving (Show, Generic)
instance FromJSON Geo

data Address
    = Address
    { street  :: Maybe String
    , suit    :: Maybe String
    , city    :: Maybe String
    , zipcode :: Maybe String
    , geo     :: Maybe Geo
    }
    deriving (Show, Generic)
instance FromJSON Address

data Company
    = Company
    { name        :: Maybe String
    , catchPhrase :: Maybe String
    , bs          :: Maybe String
    }
    deriving (Show, Generic)
instance FromJSON Company

data User
    = User
    { id       :: Maybe Int
    , name     :: Maybe String
    , username :: Maybe String
    , email    :: Maybe String
    , address  :: Maybe Address
    , phone    :: Maybe String
    , website  :: Maybe String
    , company  :: Maybe Company
    }
    deriving (Show, Generic)
instance FromJSON User

{- The following is JSON structured and is the output of the API for user 4 out of the 10.

{
  "id": 4,
  "name": "Patricia Lebsack",
  "username": "Karianne",
  "email": "Julianne.OConner@kory.org",
  "address": {
    "street": "Hoeger Mall",
    "suite": "Apt. 692",
    "city": "South Elvis",
    "zipcode": "53919-4257",
    "geo": {
      "lat": "29.4572",
      "lng": "-164.2990"
    }
  },
  "phone": "493-170-9623 x156",
  "website": "kale.biz",
  "company": {
    "name": "Robel-Corkery",
    "catchPhrase": "Multi-tiered zero tolerance productivity",
    "bs": "transition cutting-edge web services"
  }
}

-}

test :: IO ()
test = do
    userJson <- LBS.readFile "user.json"
    let maybeUser = decode @User userJson
    print maybeUser

getUserFromAPI :: Int -> IO LBS.ByteString
getUserFromAPI n = do if n `Prelude.elem` [1..10] 
                      then do json <- simpleHttp $ "https://jsonplaceholder.typicode.com/users/" ++ (show n) 
                              return json
                      else return "given integer is not in the range [1..10]"

-- Question 5
-- Replace this main function to get an user number between [1..10] and return return the parsed User from the api.

main :: IO ()
main = do
    lsMaybeUser <- for_1_10 $ decodeUser . getUserFromAPI
    forM_ lsMaybeUser $ \maybeUser -> do     -- Control.Monad
        print $ fromJust maybeUser
        Prelude.putStrLn ""
    where
        for_1_10   = forConcurrently [1..10] -- async (Control.Concurrent.Async)
        decodeUser = (<$>) $ decode @User    -- aeson
