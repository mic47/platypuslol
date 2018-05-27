module Platypuslol.Commands
  ( commands
  , defaultCommand
  , queryParser
  , QueryLang(..)
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
      query <- word "<WORD>"
      pure (query, [(pack w, pack query)])
    QueryString w -> do
      query <- case qs of
        [] -> eatAll "<QUERY>"
        _ -> anyString "<QUERY>"
      pure (query, [(pack w, pack query)])
  _ <- spaceType
  parse' <- queryParser qs
  pure $
    (fst parse:fst parse', snd parse ++ snd parse')
    where
      spaceType = case (q, qs) of
        -- There is always space before query.
        (_, QueryString{}:_) -> space1
        (_, QueryWord{}:_) -> space1
        -- There is always space after query, if non-query follows.
        (QueryWord{}, OptionalWord{}:_) -> space1
        (QueryWord{}, PrefixWord{}:_) -> space1
        (QueryString{}, OptionalWord{}:_) -> space1
        (QueryString{}, PrefixWord{}:_) -> space1
        _ -> space

queryParser' :: [QueryLang] -> AmbiguousParser (String, [(Text, Text)])
queryParser' = fmap (\(x, a) -> (mconcat $ intersperse " " x, a)) . queryParser

simpleRedirect :: String -> Text -> Command
simpleRedirect command redirectTemplate = mkCommand
  (queryParser' $ map toQueryLang $ words command)
  (mconcat . intersperse "|" . map snd)
  (urlRedirect redirectTemplate)

commands :: [(String, Text)] -> Command
commands config = anyOf $ map (uncurry simpleRedirect) config
