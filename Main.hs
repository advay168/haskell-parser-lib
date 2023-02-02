module Main where

import Parser

main = do
  line <- getLine
  print $ parse expr line

expr :: Parser Double
expr =
  do
    x <- term
    ws $ char '+'
    y <- expr
    return (x + y)
    <|> term

term :: Parser Double
term =
  do
    x <- power
    opr <- ws $ char '*' <|> char '/'
    y <- term
    return (if opr == '*' then x * y else x / y)
    <|> power

power :: Parser Double
power =
  do
    x <- factor
    ws $ char '^'
    y <- power
    return (x ** y)
    <|> factor

factor :: Parser Double
factor =
  decimal <|> do
    char '('
    x <- ws expr
    char ')'
    return x
