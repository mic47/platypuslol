module Platypuslol.RedirectServer
  ( redirectServer
  ) where

import Data.Aeson
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad.STM
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Lazy as LBS
import Text.Blaze.Html.Renderer.Text
import Network.Wai
import Network.Wai.Parse
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as Set
import Data.Monoid
import Network.HTTP.Types (status400, status404, status302, status200)
import Data.List
import Data.Text (Text, unpack, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Text.Blaze

import Platypuslol.AmbiguousParser
import Platypuslol.CommandStore
import Platypuslol.Types
import Platypuslol.Util
import Platypuslol.Web.List

import Paths_platypuslol

redirectServer
  :: (Text -> Action)
  -> (Text, Text)
  -> CommandStore
  -> FilePath
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
-- TODO: this is getting out of hand. There should be reader monad so that
-- request have access to all necessary data.
redirectServer defaultRedirect (urlPrefix, defServer) commandStore configDir req respond = do
    -- print req
    commands <- atomically $ getCommandParser commandStore
    (params', _files) <- parseRequestBodyEx
      (setMaxRequestFileSize (1024*1024) defaultParseRequestBodyOptions)
      lbsBackEnd
      req
    let
      params = HashMap.fromList $ map (\(x, y) -> (decodeUtf8 x, decodeUtf8 y)) params'
      hostUrl = mconcat
        [ urlPrefix
        , maybe defServer decodeUtf8 $ HashMap.lookup
          "Host"
          (HashMap.fromList (requestHeaders req))
        ]
      allParams = map toText (queryString req ++ map (fmap Just) params')
    response <- case pathInfo req of
      ["redirect"] -> return $ redirectCommand
        defaultRedirect
        commands
        allParams
      -- TODO: do that for redirecti/suggest with different param
      ["suggest"] -> return $ suggestCommand
        commands
        allParams
      ["debug"] -> return $ debugCommand
        commands
        allParams
      ["list"] -> listCommands commands params configDir
      [icon] | Set.member icon icons -> returnIcon icon
      [] -> installResponse
        hostUrl
        []
      ("install": path) -> installResponse
        hostUrl
        path
      _ -> return $ redirectCommand
        defaultRedirect
        commands
        [ ( "q"
          , Just $ urlDecodeText $ toQueryString
              (decodeUtf8 $ rawPathInfo req)
              (decodeUtf8 $ rawQueryString req)
          )
        ]
    respond response
  where
    toText (x, y) = (decodeUtf8 x, decodeUtf8 <$> y)
    toQueryString path params = Text.dropWhile ('/'==) (path <> params)
    icons = Set.fromList
      [ "favicon-16x16.png"
      , "favicon-32x32.png"
      , "favicon-96x96.png"
      , "favicon.ico"
      ]

debugCommand
  :: Command
  -> [(Text, Maybe Text)]
  -> Response
debugCommand commands [("q", Just query)] = do
  let actions = parseAll commands (unpack query)
  responseBuilder
    status200
    [("Content-Type", "application/json")]
    ((fromText . decodeUtf8 . LBS.toStrict . encode . toJSON) actions)


suggestCommand
  :: Command
  -> [(Text, Maybe Text)]
  -> Response
suggestCommand commands [("q", Just query)] = responseBuilder
  status200
  [("Content-Type", "application/x-suggestions+json")]
  $ fromText $
    "[\"" <> query <> "\",["
    <> mconcat
      ( intersperse ","
      $ map (pack . show . parsedText)
      $ take 20
      $ tryAgain $ parseThenSuggest commands (unpack query)
      )
    <> "]]"
  where
    -- Firefox prepends keyword. Let's drop it.
    tryAgain [] = parseThenSuggest commands (dropWhile (==' ') $ dropWhile (/=' ') $ unpack query)
    tryAgain x = x
suggestCommand _ _ = wrongQuery

redirectCommand
  :: (Text -> Action)
  -> Command
  -> [(Text, Maybe Text)]
  -> Response
redirectCommand defaultResponse commands [("q", Just query)] =
  case parseAll commands (unpack query) of
    [] -> actionToResponse $ defaultResponse query
    [x] -> actionToResponse $ parsedAction x
    responses -> selectActionResponse responses
redirectCommand _ _ _ = wrongQuery

selectActionResponse :: [ParsedCommand] -> Response
selectActionResponse actions = responseBuilder
  status200
  [("Content-Type", "text/html")]
  $ fromText $ toStrict $ renderHtml $ H.docTypeHtml $ do
    H.head $ H.title "Multiple matches"
    H.body $ do
      H.p "Too many matches. Please select the right query."
      H.ul $ mapM_ (H.li . toLink) actions
  where
    toLink (ParsedCommand name params (UrlRedirect destination)) =
      H.a
        (H.toHtml $
          "Redirect action \"" <> pack name <> "\" with query \"" <> params <> "\""
        )
        ! A.href (textValue destination)

installResponse :: Text -> [Text] -> IO Response
installResponse serverName [] = do
  installTemplateFile <- getDataFileName "resources/index.html"
  installTemplate <- Text.readFile installTemplateFile
  return $ responseBuilder
    status200
    [("Content-Type", "text/html")]
    (fromText $ Text.replace "{server}" serverName installTemplate)
installResponse serverName ["opensearch.xml"] = do
  opensearchTemplateFile <- getDataFileName "resources/opensearch.xml"
  opensearchTemplate <- Text.readFile opensearchTemplateFile
  return $ responseBuilder
    status200
    [("Content-Type", "application/opensearchdescription+xml")]
    (fromText $ Text.replace "{server}" serverName opensearchTemplate)
installResponse _ _ = return notFound

returnIcon :: Text -> IO Response
returnIcon icon = do
  fileName <- getDataFileName ("resources/" <> unpack icon)
  return $ responseFile
    status200
    [ ("Content-Type", "image/png")
    ]
    fileName
    Nothing

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

