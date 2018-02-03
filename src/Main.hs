{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Database.MongoDB
import Data.Bson
import Data.Text (Text)
import Control.Monad.IO.Class

data Person = Person {
    name::String,
    age::Int
  } deriving (Show, Generic)

instance ToJSON Person where

instance FromJSON Person where

-- TODO
data Requests = CreateRequest Person

personCollection :: Text
personCollection = "persons"

main :: IO ()
main = do
  let port = 8000
  putStrLn $ "Server is listening on port " ++ (show port) ++ "..."
  pipe <- connect (host "127.0.0.1")
  run port (webApp $ access pipe master "web-app")

fetchPersons :: Action IO [Maybe Person]
fetchPersons = fmap (map parsePersonFromDocument) (rest =<< find (select [] personCollection))

-- insertPerson :: Person -> Action IO Data.Bson.Value
insertPerson person = insert personCollection (fromPersonToDocument person) >> return [return person]

-- webApp :: (Action IO a -> IO a) -> Application
webApp db request respond =
  case (pathInfo request, requestMethod request) of
    (["person"], "GET") -> do
      results <- db fetchPersons
      let persons = fromMaybe [] (sequence results)
      respond $ response $ encode persons

    (["person"], "POST") -> do
      mayBePerson <- parsePersonBody request
      case mayBePerson of
        Just person -> do
          db (insertPerson person)
          respond $ response $ encode person
        Nothing -> respond $ responseLBS status422 [] "Cannot deserialize body to Person { name::String, age::Int })"

    (["echo", word], _) -> respond $ response $ pack ("{\"echo\": " ++ (show word) ++ "}")

    _ -> respond $ response (pack "{\"message\": \"no match\"}")

response :: ByteString -> Response
response body =
  responseLBS status200 [("Content-Type", "application/json")] body

parsePersonFromDocument :: Document -> Maybe Person
parsePersonFromDocument document = do
  name <- Database.MongoDB.lookup "name" document :: Maybe String
  age <- Database.MongoDB.lookup "age" document :: Maybe Int
  return (Person name age)

fromPersonToDocument :: Person -> Document
fromPersonToDocument (Person name age) = ["name" :=  val name, "age" := val age]

parsePersonBody :: Request -> IO (Maybe Person)
parsePersonBody request =
  fmap decode (strictRequestBody request)