module Main where

import Parser

main = do
  line <- getLine
  print $ parse expr line

expr :: Parser Double
expr =
  do
    x <- term
    whitespace
    char '+'
    whitespace
    y <- term
    return (x + y)
    <|> term

term :: Parser Double
term =
  do
    x <- power
    whitespace
    opr <- char '*' <|> char '/'
    y <- power
    whitespace
    return (if opr == '*' then x * y else x / y)
    <|> power

power :: Parser Double
power =
  do
    x <- factor
    whitespace
    char '^'
    y <- factor
    whitespace
    return (x ** y)
    <|> factor

factor :: Parser Double
factor =
  decimal <|> do
    char '('
    whitespace
    x <- expr
    char ')'
    return x