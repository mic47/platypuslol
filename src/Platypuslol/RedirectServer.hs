module Platypuslol.RedirectServer 
  ( redirectServer
  ) where

import Network.Wai
import Network.HTTP.Types (status400, status404)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Strict as HashMap

import Platypuslol.Types

redirectServer 
  :: Command
  -> Commands
  -> Request 
  -> (Response -> IO ResponseReceived) 
  -> IO ResponseReceived
redirectServer defaultCommand commands req respond = respond $
  case pathInfo req of
    ["redirect"] -> redirectCommand 
      defaultCommand
      commands
      (map toText $ queryString req)
    _ -> notFound
  where
    toText (x, y) = (decodeUtf8 x, decodeUtf8 <$> y)

redirectCommand 
  :: Command
  -> Commands 
  -> [(Text, Maybe Text)] 
  -> Response
redirectCommand defaultCommand commands [("q", Just query)] = case tokenized of
  (x:_) -> fromMaybe  
    runDefaultCommand
    ( (commandAction <$> (HashMap.lookup x commands))
      <*> return tokenized
    )
  _ -> runDefaultCommand
  where
    runDefaultCommand = commandAction defaultCommand ("":tokenized)
    tokenized = Text.splitOn " " query
redirectCommand _ _ _ = wrongQuery

wrongQuery :: Response
wrongQuery = responseBuilder
  status400 
  [ ("Content-Type", "text/plain")
  ]
  "Query does not make sense."

notFound :: Response
notFound = responseBuilder
  status404
  [ ("Content-Type", "text/plain")
  ]
  "Page not found."
