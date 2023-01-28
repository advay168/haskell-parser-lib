module Parser
  ( Parser,
    parse,
    char,
    digit,
    natural,
    integer,
    decimal,
    whitespace,
    (<|>),
    some,
    many,
  )
where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe

newtype Parser a = Parser (String -> Maybe (a, String))

instance Functor Parser where
  f `fmap` Parser p = Parser $ \text ->
    do
      (val, r) <- p text
      return (f val, r)

instance Applicative Parser where
  pure val = Parser $ \text -> pure (val, text)
  Parser p1 <*> Parser p2 = Parser $ \text ->
    do
      (f, r1) <- p1 text
      (val, r2) <- p2 r1
      return (f val, r2)

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \text ->
    do
      (v, r) <- p text
      let Parser p1 = f v
      p1 r

instance Alternative Parser where
  empty = Parser $ const empty
  Parser p1 <|> Parser p2 = Parser $ \text -> p1 text <|> p2 text

instance MonadPlus Parser

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) = p

whitespace :: Parser ()
whitespace = void $ many (char ' ')

char :: Char -> Parser Char
char c = Parser func
  where
    func [] = empty
    func (cx : cs) = if cx == c then pure (cx, cs) else empty

digit :: Parser Int
digit = Parser func
  where
    func [] = empty
    func (cx : cs) = if cx `elem` ['0' .. '9'] then pure (read [cx], cs) else empty

natural :: Parser Int
natural = foldl (\acc x -> acc * 10 + x) 0 <$> some digit

integer :: Parser Int
integer = natural <|> (char '-' >> natural <&> negate)

decimal :: Parser Double
decimal =
  do
    intPart <- fromMaybe 0 <$> optional integer
    char '.'
    fracPart <- natural
    return (fromIntegral intPart + until (< 1) (/ 10) (fromIntegral fracPart))
    <|> (fromIntegral <$> integer)
