module Platypuslol.RedirectServer 
  ( redirectServer
  ) where

import Network.Wai
import Network.HTTP.Types (status400, status404)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)

import Platypuslol.AmbiguousParser
import Platypuslol.Types

redirectServer
  :: (Text -> Response)
  -> Command
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
redirectServer defaultRedirect commands req respond = respond $ 
  case pathInfo req of
    ["redirect"] -> redirectCommand
      defaultRedirect
      commands
      (map toText $ queryString req)
    _ -> notFound
  where
    toText (x, y) = (decodeUtf8 x, decodeUtf8 <$> y)

redirectCommand
  :: (Text -> Response)
  -> Command
  -> [(Text, Maybe Text)]
  -> Response
redirectCommand defaultResponse commands [("q", Just query)] = 
  case parseAll commands (unpack query) of
    [] -> defaultResponse query
    ((_, _, x):_) -> x -- TODO: warn about multiple options
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
