module Platypuslol.Commands
  ( commands
  , defaultCommand
  ) where

import Data.List
import Data.Text (Text, pack)

import Platypuslol.AmbiguousParser
import Platypuslol.Types

defaultCommand :: Text -> Action
defaultCommand query = urlRedirect
  "https://www.google.com/search?q={query}"
  [("{query}", query)]

-- TODO: maybe add non-prefix words?
data QueryLang
  = PrefixWord String
  | OptionalWord String
  | QueryString String
  | QueryWord String
  deriving (Show)

-- TODO: use proper parser, allow escaping, but this is good start
toQueryLang :: String -> QueryLang
toQueryLang w@('{':_) = QueryString w
toQueryLang w@('[':_) = OptionalWord w
toQueryLang w@('<':_) = QueryWord w
toQueryLang w = PrefixWord w

queryParser :: [QueryLang] -> AmbiguousParser ([String], [(Text, Text)])
queryParser [] = pure ([], [])
queryParser (q:qs) = do
  parse <- case q of
    PrefixWord w -> (, []) <$> prefix w
    OptionalWord w -> (, []) <$> anyOf [string w, pure ""]
    QueryWord w -> do
      _ <- char ' '
      query <- word
      pure (query, [(pack w, pack query)])
    QueryString w -> do
      _ <- char ' '
      query <- case qs of
        [] -> eatAll
        _ -> anyString
      pure (query, [(pack w, pack query)])
  -- TODO: solve problem with whitespace between query and non-query 
  _ <- many $ char ' '
  parse' <- queryParser qs
  pure $
    (fst parse:fst parse', snd parse ++ snd parse')

queryParser' :: [QueryLang] -> AmbiguousParser (String, [(Text, Text)])
queryParser' = fmap (\(x, a) -> (mconcat $ intersperse " " x, a)) . queryParser

simpleRedirect :: String -> Text -> Command
simpleRedirect command redirectTemplate = mkCommand
  (queryParser' $ map toQueryLang $ words command)
  (mconcat . intersperse "|" . map snd)
  (urlRedirect redirectTemplate)

commands :: [(String, Text)] -> Command
commands fromConfig = anyOf $ map (uncurry simpleRedirect) $
  [ ( "google {query}"
    , "https://www.google.com/search?q={query}"
    )
  , ( "hoogle {query}"
    , "https://www.stackage.org/lts-10.8/hoogle?q={query}"
    )
  , ( "local hoogle {query}"
    , "http://localhost:8080/?hoogle={query}"
    )
  , ( "search scala docs {query}"
    , "https://www.scala-lang.org/api/current/scala/?search={query}"
    )
  , ( "search scala types {query}"
    , "http://scala-search.org/?m=org.scala-lang%3Ascala-library%3A2.11.7&m=org.scalaz%3Ascalaz-core_2.11%3A7.1.1&q={query}"
    )
  ] ++ fromConfig
