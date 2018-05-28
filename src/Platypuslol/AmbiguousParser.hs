module Platypuslol.AmbiguousParser
  ( AmbiguousParser
  , parseAll
  , suggestAll
  , setSuggestion
  , char
  , string
  , prefix
  , option
  , anyOf
  , many
  , many1
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

newtype AmbiguousParser a = AmbiguousParser
  { parse :: String -> [(Either a a, String)]
  }

parseAll :: AmbiguousParser a -> String -> [a]
parseAll (AmbiguousParser p) =
  mapMaybe (rightToMaybe . fst)
  . filter (("" ==) . snd)
  . filter (isRight . fst)
  . p
  where
    rightToMaybe (Right x) = Just x
    rightToMaybe Left{} = Nothing

suggestAll :: AmbiguousParser a -> String -> [a]
suggestAll (AmbiguousParser p) =
  mapMaybe (leftToMaybe . fst)
  . filter (("" ==) . snd)
  . filter (isLeft . fst)
  . p
  where
    leftToMaybe (Left a) = Just a
    leftToMaybe Right{} = Nothing

setSuggestion :: a -> AmbiguousParser a -> AmbiguousParser a
setSuggestion suggestion (AmbiguousParser p) = AmbiguousParser $ \case
  "" -> [(Left suggestion, "")]
  x ->
    let
      (lefts', rights') = partition (isLeft . fst) (p x)
    in
      map (first (const (Left suggestion))) (take 1 lefts') ++ rights'

pickMin
  :: ((Either a a, String) -> (Either a a, String) -> Ordering)
  -> AmbiguousParser a
  -> AmbiguousParser a
pickMin cmp (AmbiguousParser p) = AmbiguousParser $ take 1 . sortBy cmp . p

ateMore
  :: (Either (Int, a) (Int, a), String)
  -> (Either (Int, a) (Int, a), String)
  -> Ordering
ateMore (Right a, _) (Right b, _) = compare (fst b) (fst a)
ateMore (Right{}, _) _ = LT
ateMore (Left a, _) (Left b, _) = compare (fst b) (fst a)
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
      f' (Left a) = Left $ f a
      f' (Right a) = Right $ f a

instance Applicative AmbiguousParser where
  pure x = AmbiguousParser $ \a -> [(Right x, a)]
  (AmbiguousParser pf) <*> (AmbiguousParser px) = AmbiguousParser $ \s ->
    [ (f' f x, rest2)
    | (f, rest1) <- pf s
    , (x, rest2) <- px rest1
    ]
    where
      f' (Right f) (Right x) = Right $ f x
      -- If any is Left, then result is Left
      f' (Left f) (Left x) = Left $ f x
      f' (Left f) (Right x) = Left $ f x
      f' (Right f) (Left x) = Left $ f x


option :: AmbiguousParser a -> AmbiguousParser a -> AmbiguousParser a
option p q = anyOf [p, q]

char :: Char -> AmbiguousParser Char
char c = AmbiguousParser
  (\case
    x:xs | x == c -> [(Right x, xs)]
    [] -> [(Left c, [])]
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
    (\x -> isRight (fst x) && "" /= snd x)
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

prefix :: String -> AmbiguousParser String
prefix x = setSuggestion x $ anyOf $ map (fmap (const x) . string) $ reverse $ drop 1 $ inits x

eatAll :: String -> AmbiguousParser String
eatAll suggestion = AmbiguousParser $ \case
  "" -> [(Left suggestion, "")]
  x -> [(Right x, "")]

word :: String -> AmbiguousParser String
word suggestion = AmbiguousParser $ \case
  "" -> [(Left suggestion, "")]
  x ->
    [( Right $ takeWhile (/= ' ') x
    , drop 1 $ dropWhile (/= ' ') x
    )]

anyString :: String -> AmbiguousParser String
anyString suggestion = AmbiguousParser $ \case
  "" -> [(Left suggestion, "")]
  x -> map (first Right) (splits x)

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
