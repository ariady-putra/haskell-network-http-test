{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

module Main where

import System.IO

{- HTTP
import Network.HTTP
-}
import Network.HTTP.Simple -- http-conduit

import GHC.Generics

import qualified Data.Text as Text
import qualified Data.Scientific as Scientific

-- unordered-containers
-- import qualified Data.HashMap.Strict as SHM

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString.Char8 as B8

import Data.Aeson -- aeson
-- import Data.Aeson.Types
import qualified Data.Aeson.KeyMap as KMA

{- forM
import Control.Monad
-}
import Control.Monad.IO.Class

-- import Control.Lens

import Control.Concurrent.Async

data Country
    = Country
    { country_id :: String
    , probability :: Double
    }
    deriving (Show, Generic)
instance FromJSON Country

data Person
    = Person
    { name        :: String
    -- , gender      :: String
    -- , probability :: Float
    -- , age         :: Int
    , country     :: [Country]
    -- , count       :: Int
    }
    deriving (Show, Generic)
-- makeLenses ''Person

instance FromJSON Person -- where
    -- parseJSON = withObject "Person" $ \ v -> Person
    --     <$> v .: "name"
    --     <*> v .: "age"
    --     <*> v .: "count"
    
    -- parseJSON (Object v) = Person
    --     <$> v .: "name"
    --     <*> v .: "age"
    --     <*> v .: "count"
    -- parseJSON invalid =
    --     prependFailure "Parsing Person failed, "
    --         (typeMismatch "Object" invalid)

getPerson :: (MonadIO io) => String -> io (Maybe Person)
getPerson name = do
    let request = "https://api.nationalize.io?name=" <> name
    -- let request = "https://api.genderize.io?name=" <> name
    liftIO . putStrLn $ request
    response <- httpLBS . parseRequest_ $ request
    -- response <- httpBS $ "https://api.agify.io?name=Ariady"
    let body = getResponseBody response
    -- B8.putStrLn body
    liftIO . L8.putStrLn $ "getPerson :: body = " <> body
    
    {- Network.HTTP.simpleHTTP -}
    -- response <- simpleHTTP . getRequest $ "https://api.agify.io?name=Ariady"
    -- print response
    -- body <- getResponseBody $ response
    -- print body
    
    {- Maybe Data.Aeson.Types.Internal.Value -}
    -- let mValue = decodeStrict body :: Maybe Value
    -- case mValue of
    --     Just (Object fList) -> do
    --         let mName = KMA.lookup "name" fList
    --             mAge  = KMA.lookup "age"  fList
    --         case (mName, mAge) of
    --             (Just (String tName), Just (Number sAge)) -> do
    --                 let name = Text.unpack tName
    --                 putStrLn $ name ++ "'s age is " ++
    --                     case Scientific.toBoundedInteger sAge :: Maybe Int of
    --                         Just age -> show age
    --                         _ -> show sAge
    --             _ -> print fList
    --     _ -> print response
    
    {- Control.Lens Person -}
    -- let mPerson = decodeStrict body :: Maybe Person
    -- case mPerson of
    --     Just person -> do
    --         let name = view pName person
    --             age  = view pAge  person
    --         putStrLn $ name ++ "'s age is " ++ show age
    --     _ -> print response
    
    let mPerson = decode body -- :: Maybe Person
    liftIO . putStrLn $ "getPerson :: " ++
        case mPerson of
            Just (Person n {- g p {- a -} -} c) ->
                n ++ " = " {- ++ {- show a ++ " = " ++ -} g ++ " = " ++ show p -}
                ++ show c
            _ -> "response = " ++ show response
    return mPerson

names =
    [ "Kiki"
    , "Putra"
    ]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Name = "
    name <- getLine
    -- person <- getPerson name
    -- print person
    -- fName <- replicateConcurrently 5 $ do
    --     putStr "Name = "
    --     getLine
    -- lName <- replicateConcurrently 5 $ return names
    -- let allNames = fName ++ concat lName
    mPersons <- mapConcurrently getPerson
        (replicate 125 name ++ concat
        (replicate 125 names)
        )
    -- lmPerson <- forM (name:names) getPerson
    case sequenceA mPersons of
        Just persons -> do
            mapM_ print persons
            putStrLn $ "persons count = " ++ show (length persons)
        _ -> do
            print mPersons
            putStrLn $ "mPersons count = " ++ show (length mPersons)
        
    
