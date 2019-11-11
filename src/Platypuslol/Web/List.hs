module Platypuslol.Web.List
  ( listCommands
  ) where

import Blaze.ByteString.Builder.Char.Utf8
import Control.Exception
import Control.Monad
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Network.Wai
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Types (status200)
import Data.List
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Text.Blaze
import TextShow (showt)
import System.FilePath.Posix

import Platypuslol.AmbiguousParser
import Platypuslol.Types
import Platypuslol.Util

type PostParams = HashMap.HashMap Text Text

listCommands :: Command -> PostParams -> FilePath -> IO Response
listCommands commands params configDir = do
  (message, params') <- updateConfig params configDir
  return $ responseBuilder
    status200
    [("Content-Type", "text/html")]
    $ fromText $ toStrict $ renderHtml $ H.docTypeHtml $ do
      H.head $ H.title "List of available commands"
      H.body $ do
        listOfCommands commands
        addCommandForm params' message

updateConfig :: PostParams -> FilePath -> IO (Maybe Text, PostParams)
updateConfig params configDir = case HashMap.lookup "submit" params of
  Nothing -> return (Nothing, params)
  Just{} -> case requestData of
    Nothing -> return
      ( Just "You have to submit all parameters"
      , params
      )
    Just redirect ->
      (do
        -- TODO: this should be done at least with file locks,
        -- or in other atomic way.
        db <- (read <$> readFile config)
          `catch` \(_ :: SomeException) -> return []
        writeFile config $ show $ redirect : db
        return
          ( Just "New command successfully added!"
          , HashMap.empty)
      ) `catch` \(e :: SomeException) -> do
        print e
        return
          ( Just $ "Error while adding command:" <> showt e
          , params
          )

  where
    config = configDir </> "db.conf"
    requestData = do
      query <- HashMap.lookup "query_string" params
      redirect <- HashMap.lookup "redirect_string" params
      guard (query /= "")
      guard (redirect /= "")
      return (query, redirect)

listOfCommands :: Command -> H.Markup
listOfCommands commands = do
  H.p "List of available commands:"
  H.ul $ mapM_
    (H.li . toLink)
    (sort $ map parsedText $ suggestAll commands "")
  where
    toLink query = H.a
      (H.toHtml query)
      ! A.href (textValue $ "redirect?q=" <> urlEncodeText (pack query))

addCommandForm :: PostParams -> Maybe Text -> H.Markup
addCommandForm params message = do
  H.p "Missing something? Add it!"
  H.form
    ! A.method "post"
    $ do
      H.p "search string:"
      H.input
        ! A.type_ "text"
        ! A.name "query_string"
        ! A.value (H.textValue $ HashMap.lookupDefault "" "query_string" params)
      H.br
      H.p "redirect string:"
      H.input
        ! A.type_ "text"
        ! A.name "redirect_string"
        ! A.value (H.textValue $ HashMap.lookupDefault "" "redirect_string" params)
      H.br
      H.input
        ! A.type_ "submit"
        ! A.name "submit"
        ! A.value "Submit"
      case message of
        Just p -> H.p
          (H.text p)
        Nothing -> return ()
