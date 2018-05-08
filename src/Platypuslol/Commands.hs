module Platypuslol.Commands
  ( commands
  , defaultCommand
  ) where

import Data.Text (Text)
import Network.Wai

import Platypuslol.AmbiguousParser
import Platypuslol.Types

defaultCommand :: Text -> Response
defaultCommand = urlRedirect "https://www.google.com/search?q={query}"

simpleRedirect :: String -> Text -> Command
simpleRedirect command redirectTemplate = mkCommand
  (singleParam $ prefixSentence' command)
  id
  (urlRedirect redirectTemplate)

commands :: [(String, Text)] -> Command
commands fromConfig = anyOf $ map (uncurry simpleRedirect) $
  [ ( "google"
    , "https://www.google.com/search?q={query}"
    )
  , ( "hoogle"
    , "https://www.stackage.org/lts-10.8/hoogle?q={query}"
    )
  , ( "local hoogle"
    , "http://localhost:8080/?hoogle={query}"
    )
  , ( "search scala docs"
    , "https://www.scala-lang.org/api/current/scala/?search={query}"
    )
  , ( "search scala types"
    , "http://scala-search.org/?m=org.scala-lang%3Ascala-library%3A2.11.7&m=org.scalaz%3Ascalaz-core_2.11%3A7.1.1&q={query}"
    )
  ] ++ fromConfig
