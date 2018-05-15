module Platypuslol.RedirectServer
  ( redirectServer
  ) where

import Blaze.ByteString.Builder.Char.Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Network.Wai
import Data.Monoid
import Network.HTTP.Types (status400, status404, status302, status200)
import Data.List
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Text.Blaze

import Platypuslol.AmbiguousParser
import Platypuslol.Types

import Debug.Trace

redirectServer
  :: (Text -> Action)
  -> Command
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
redirectServer defaultRedirect commands req respond = do
  -- putStrLn $ show req
  respond $
    case pathInfo req of
      ["redirect"] -> redirectCommand
        defaultRedirect
        commands
        (map toText $ queryString req)
      -- TODO: do that for redirecti/suggest with different param
      ["suggest"] -> suggestCommand
        defaultRedirect
        commands
        (map toText $ queryString req)
      _ -> notFound
  where
    toText (x, y) = (decodeUtf8 x, decodeUtf8 <$> y)

suggestCommand
  :: (Text -> Action)
  -> Command
  -> [(Text, Maybe Text)]
  -> Response
suggestCommand _ commands [("q", Just query)] = responseBuilder
  status200
  [("Content-Type", "application/x-suggestions+json")]
  $ fromText $ traceShowId $
    "[\"" <> query <> "\",["
    <> mconcat (intersperse "," $ map (pack . show . (\(a, _, _) -> a)) $ take 20 $ suggestAll commands (unpack query))
    <> "]]"
suggestCommand _ _ _ = wrongQuery

redirectCommand
  :: (Text -> Action)
  -> Command
  -> [(Text, Maybe Text)]
  -> Response
redirectCommand defaultResponse commands [("q", Just query)] =
  case parseAll commands (unpack query) of
    [] -> actionToResponse $ defaultResponse query
    [(_, _, x)] -> actionToResponse x
    responses -> selectActionResponse responses
redirectCommand _ _ _ = wrongQuery

selectActionResponse :: [(String, Text, Action)] -> Response
selectActionResponse actions = responseBuilder
  status200
  [("Content-Type", "text/html")]
  $ fromText $ toStrict $ renderHtml $ H.docTypeHtml $ do
    H.head $ H.title "Multiple matches"
    H.body $ do
      H.p "Too many matches. Please select the right query."
      H.ul $ mapM_ (H.li . toLink) actions
  where
    toLink (name, params, UrlRedirect destination) =
      H.a
        (H.toHtml $
          "Redirect action \"" <> pack name <> "\" with query \"" <> params <> "\""
        )
        ! A.href (textValue destination)


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

actionToResponse :: Action -> Response
actionToResponse (UrlRedirect destination) = responseBuilder
  status302
  [ ("Location"
    , encodeUtf8 destination
    )
  ]
  mempty

