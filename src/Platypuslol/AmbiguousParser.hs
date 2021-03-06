module Platypuslol.AmbiguousParser
  ( AmbiguousParser
  , parseAll
  , suggestAll
  , parseThenSuggest
  , setSuggestion
  , subsequenceWord
  , predicateWord
  , char
  , string
  , prefix
  , option
  , anyOf
  , many
  , many1
  , suggestInstead
  , eatAll
  , word
  , anyString
  , prefixSentence
  , prefixSentence'
  , pickLongestMatch
  , space
  , space1
  , spaced
  , separated
  ) where

import Control.Arrow
import Data.List
import Data.Either
import Data.Maybe

newtype Score = Score Double
  deriving (Show, Eq, Ord)

instance Semigroup Score where
  (<>) (Score a) (Score b) = Score (a * b)

instance Monoid Score where
  mempty = Score 1.0
  mappend = (<>)

data ParserResult a
  = Parsed a Score
  | Suggested a
  deriving (Show)

isParsed :: ParserResult a -> Bool
isParsed Parsed{} = True
isParsed Suggested{} = False

isSuggested :: ParserResult a -> Bool
isSuggested Parsed{} = False
isSuggested Suggested{} = True

partitionParserResult :: [ParserResult a] -> ([a], [a])
partitionParserResult = fmap sortResults . partitionEithers . map pToEither
  where
    pToEither (Parsed x s) = Right (x, s)
    pToEither (Suggested x) = Left x

sortResults :: [(a, Score)] -> [a]
sortResults = map fst . sortOn snd

newtype AmbiguousParser a = AmbiguousParser
  { parse :: String -> [(ParserResult a, String)]
  }

parseAll :: AmbiguousParser a -> String -> [a]
parseAll (AmbiguousParser p) =
  sortResults
  . mapMaybe (rightToMaybe . fst)
  . filter (("" ==) . snd)
  . filter (isParsed . fst)
  . p
  where
    rightToMaybe (Parsed x s) = Just (x, s)
    rightToMaybe Suggested{} = Nothing

parseThenSuggest :: AmbiguousParser a -> String -> [a]
parseThenSuggest (AmbiguousParser p) =
  toList
  . partitionParserResult
  . map fst
  . filter ((""==) . snd)
  . p
  where
    toList (l, r) = r ++ l


suggestAll :: AmbiguousParser a -> String -> [a]
suggestAll (AmbiguousParser p) =
  mapMaybe (leftToMaybe . fst)
  . filter (("" ==) . snd)
  . filter (isSuggested . fst)
  . p
  where
    leftToMaybe (Suggested a) = Just a
    leftToMaybe Parsed{} = Nothing

setSuggestion :: a -> AmbiguousParser a -> AmbiguousParser a
setSuggestion suggestion (AmbiguousParser p) = AmbiguousParser $ \case
  "" -> [(Suggested suggestion, "")]
  x ->
    let
      (lefts', rights') = partition (isSuggested . fst) (p x)
    in
      map (first (const (Suggested suggestion))) (take 1 lefts') ++ rights'

pickMin
  :: ((ParserResult a, String) -> (ParserResult a, String) -> Ordering)
  -> AmbiguousParser a
  -> AmbiguousParser a
pickMin cmp (AmbiguousParser p) = AmbiguousParser $ take 1 . sortBy cmp . p

ateMore
  :: (ParserResult (Int, a), String)
  -> (ParserResult (Int, a), String)
  -> Ordering
ateMore (Parsed a _, _) (Parsed b _, _) = compare (fst b) (fst a)
ateMore (Parsed{}, _) _ = LT
ateMore (Suggested a, _) (Suggested b, _) = compare (fst b) (fst a)
ateMore _ _ = GT

pickLongestMatch :: AmbiguousParser [a] -> AmbiguousParser [a]
pickLongestMatch parser =
  fmap snd
  $ pickMin ateMore
  $ fmap (\x -> (length x, x)) parser

space, space1 :: AmbiguousParser String
space = pickLongestMatch $ many $ char ' '
space1 = pickLongestMatch $ many1 $ char ' '

instance Functor AmbiguousParser where
  fmap f (AmbiguousParser p) = AmbiguousParser $ map (first f') . p
    where
      f' (Suggested a) = Suggested $ f a
      f' (Parsed a x) = Parsed (f a) x

instance Applicative AmbiguousParser where
  pure x = AmbiguousParser $ \a -> [(Parsed x mempty, a)]
  (AmbiguousParser pf) <*> (AmbiguousParser px) = AmbiguousParser $ \s ->
    [ (f' f x, rest2)
    | (f, rest1) <- pf s
    , (x, rest2) <- px rest1
    ]
    where
      f' (Parsed f fs) (Parsed x xs) = Parsed (f x) (fs <> xs)
      -- If any is Suggested, then result is Suggested
      f' (Suggested f) (Suggested x) = Suggested $ f x
      f' (Suggested f) (Parsed x _) = Suggested $ f x
      f' (Parsed f _) (Suggested x) = Suggested $ f x


option :: AmbiguousParser a -> AmbiguousParser a -> AmbiguousParser a
option p q = anyOf [p, q]

char :: Char -> AmbiguousParser Char
char c = AmbiguousParser
  (\case
    x:xs | x == c -> [(Parsed x mempty, xs)]
    [] -> [(Suggested c, [])]
    _ -> []
  )

string :: String -> AmbiguousParser String
string [] = pure []
string (x:xs) = do
  x' <- char x
  xs' <- string xs
  return $ x':xs'

anyOf :: [AmbiguousParser a] -> AmbiguousParser a
anyOf parsers = AmbiguousParser $ \s -> mconcat
  ( map
    (`parse` s)
    parsers
  )

anyOfSuggestionOnce :: [AmbiguousParser a] -> AmbiguousParser a
anyOfSuggestionOnce parsers = AmbiguousParser $ \s ->
  ( takeWhileAndOne
    (\x -> isParsed (fst x) && "" /= snd x)
    ( mconcat $ takeWhile (not . null) $ map
      (`parse` s)
      parsers
    )
  )
  where
    takeWhileAndOne p l =
      let (trues, falses) = span p (take 100 l) in trues ++ take 1 falses

repeatParser :: Int -> AmbiguousParser a -> AmbiguousParser [a]
repeatParser n parser = do
  x <- parser
  xs <- repeatParser' (n-1)
  pure (x: xs)
  where
    repeatParser' n'
      | n' <= 0 = pure []
      | otherwise = do
        x <- parser
        xs <- repeatParser' (n' - 1)
        pure (x:xs)

many :: AmbiguousParser a -> AmbiguousParser [a]
many parser = anyOfSuggestionOnce $ pure [] : map (`repeatParser` parser) [1,2..]

many1 :: AmbiguousParser a -> AmbiguousParser [a]
many1 parser = anyOfSuggestionOnce $ map (`repeatParser` parser) [1,2..]

-- | Change suggestion of this parser (but nothing nested isnside) into
-- arbitrary sggestion.
suggestInstead :: a -> AmbiguousParser a -> AmbiguousParser a
suggestInstead suggestion (AmbiguousParser p) = AmbiguousParser $ \case
  "" -> [(Suggested suggestion, "")]
  x -> p x

predicateWord :: a -> (String -> Maybe Score) -> AmbiguousParser a
predicateWord item predicate = AmbiguousParser $ \case
  "" -> [(Suggested item, "")]
  what ->
    let (wrd, rest) = span (/=' ') what
    in
      [ (Parsed item score, rest)
      | (Just score) <- [predicate wrd]
      ]

isSubsequenceAndGetSpan :: String -> String -> Maybe Int
isSubsequenceAndGetSpan needle haystack = isSubsequenceAndGetSpan'
  (zip needle (repeat Nothing))
  haystack
  0

isSubsequenceAndGetSpan' :: [(Char, Maybe Int)] -> String -> Int -> Maybe Int
isSubsequenceAndGetSpan' [] _ _ =  Just 0
isSubsequenceAndGetSpan' x [] index = (index-) <$> snd (last x)
isSubsequenceAndGetSpan' state@((firstChar, firstLen): rest) (hd:restInput) index =
  let
    updatedState =
      ( (if firstChar == hd then (firstChar, Just index) else (firstChar, firstLen))
      : zipWith stepUpdate state rest
      )
    distance = (1+) . (index -) <$> snd (last updatedState)
  in
    minState distance $ isSubsequenceAndGetSpan'
      updatedState
      restInput
      (index + 1)
  where
    stepUpdate (_, prevIndex) x@(actChar, actIndex) =
       if hd == actChar then (actChar, latestIndex prevIndex actIndex) else x
    latestIndex Nothing a = a
    latestIndex a Nothing = a
    latestIndex a b = max <$> a <*> b
    minState Nothing a = a
    minState a Nothing = a
    minState a b = min <$> a <*> b


subsequenceWord :: String -> AmbiguousParser String
subsequenceWord w = predicateWord w (`toScore` w)
  where
  toScore x y = Score <$> ((/ fromIntegral (length x)) . fromIntegral <$> x `isSubsequenceAndGetSpan` y)

prefix :: String -> AmbiguousParser String
prefix x = setSuggestion x $ anyOf $ map (fmap (const x) . string) $ reverse $ drop 1 $ inits x

eatAll :: String -> AmbiguousParser String
eatAll suggestion = AmbiguousParser $ \case
  "" -> [(Suggested suggestion, "")]
  x -> [(Parsed x mempty, "")]

word :: String -> AmbiguousParser String
word suggestion = AmbiguousParser $ \case
  "" -> [(Suggested suggestion, "")]
  x ->
    [( Parsed (takeWhile (/= ' ') x) mempty
    , drop 1 $ dropWhile (/= ' ') x
    )]

anyString :: String -> AmbiguousParser String
anyString suggestion = AmbiguousParser $ \case
  "" -> [(Suggested suggestion, "")]
  x -> map (first (`Parsed` mempty)) (splits x)

splits :: [a] -> [([a], [a])]
splits [] = []
splits (a:as) = ([a], as) : map (first (a:)) (splits as)

prefixSentence :: [String] -> AmbiguousParser [String]
prefixSentence [] = pure []
prefixSentence [x] = (:[]) <$> prefix x
prefixSentence (x:xs) = do
  x' <- prefix x
  w <- space
  xs' <- prefixSentence xs
  pure $ const (x':xs') w

prefixSentence' :: String -> AmbiguousParser String
prefixSentence' = fmap (mconcat . intersperse " ") . prefixSentence . words

spaced :: AmbiguousParser a -> AmbiguousParser b -> AmbiguousParser (a, b)
spaced p1 p2 = do
  p1' <- p1
  w <- space1
  p2' <- p2
  pure $ const (p1', p2') w

separated :: AmbiguousParser a -> [AmbiguousParser b] -> AmbiguousParser [b]
separated _ [] = pure []
separated _ [x] = fmap (:[]) x
separated separator (x:xs) = do
  x' <- x
  s <- separator
  xs' <- separated separator xs
  pure $ const (x':xs') s

