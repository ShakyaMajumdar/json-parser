{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad (guard)
import Data.Functor (($>), (<$))
import Data.Map (Map)
import Debug.Trace (trace)
import GHC.Base (Alternative (empty, (<|>)))
import System.IO (hFlush, stdout)

newtype Parser a = Parser {runParser :: String -> [(a, String)]} deriving (Functor)

instance Monad Parser where
  return v = Parser $ \inp -> [(v, inp)]
  p >>= f = Parser $ \inp -> concat [runParser (f v) inp' | (v, inp') <- runParser p inp]

instance Applicative Parser where
  pure = return
  p <*> q =
    do
      p' <- p
      p' <$> q

instance Alternative Parser where
  empty = Parser $ const []
  p <|> q = Parser $ \inp -> runParser p inp ++ runParser q inp

item :: Parser Char
item = Parser $ \case
  "" -> []
  (x : xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p =
  do
    x <- item
    guard (p x)
    return x

many :: Parser a -> Parser [a]
many p =
  ( do
      p' <- p
      ps <- many p
      return (p' : ps)
  )
    <|> return []

many1 :: Parser a -> Parser [a]
many1 p =
  do
    p' <- p
    ps <- many p
    return (p' : ps)

char :: Char -> Parser Char
char c = sat (c ==)

digit :: Parser Char
digit = sat (`elem` ['0' .. '9'])

onenine :: Parser Char
onenine = sat (`elem` ['1' .. '9'])

hex :: Parser Char
hex = digit <|> sat (`elem` ['a' .. 'f']) <|> sat (`elem` ['A' .. 'F'])

ws :: Parser String
ws = many1 (sat (`elem` ['\x0020', '\x000A', '\x000D', '\x0009'])) <|> return ""

string :: String -> Parser String
string "" = return ""
string (x : xs) =
  do
    _ <- char x
    _ <- string xs
    return (x : xs)

brackets :: Parser a -> Parser b -> Parser c -> Parser b
brackets a b c = do
  _ <- a
  b' <- b
  _ <- c
  return b'

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  p' <- p
  ps <- many $ sep >> p
  return (p' : ps)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> (ws $> [])

sign :: Parser String
sign = string "+" <|> string "-" <|> return ""

integer :: Parser String
integer =
  ( do
      _ <- char '-'
      d <- onenine
      ds <- many1 digit
      return ('-' : d : ds)
  )
    <|> ( do
            _ <- char '-'
            d <- digit
            return ('-' : d : "")
        )
    <|> ( do
            d <- onenine
            ds <- many1 digit
            return (d : ds)
        )
    <|> (return <$> digit)

fraction :: Parser String
fraction =
  ( do
      _ <- char '.'
      ds <- many1 digit
      return ('.' : ds)
  )
    <|> return ""

expo :: Parser String
expo =
  ( do
      e <- string "E" <|> string "e"
      s <- sign
      ds <- many1 digit
      return (e ++ s ++ ds)
  )
    <|> return ""

number :: Parser Double
number =
  do
    i <- integer
    f <- fraction
    e <- expo
    return (read (i ++ f ++ e) :: Double)

escape :: Parser String
escape =
  return <$> sat (`elem` ['"', '\\', '/', 'b', 'f', 'n', 'r', 't'])
    <|> ( do
            _ <- char 'u'
            h1 <- hex
            h2 <- hex
            h3 <- hex
            h4 <- hex
            return ('x' : h1 : h2 : h3 : h4 : "")
        )

character :: Parser Char
character =
  sat (not . (`elem` ['"', '\\']))
    <|> ( do
            _ <- char '\\'
            e <- escape
            return (read $ "'\\" ++ e ++ "'" :: Char)
        )

characters :: Parser String
characters = many character

member :: Parser (String, Json)
member = do
  s <- brackets ws parseJString ws
  _ <- char ':'
  e <- brackets ws parseJson ws
  return (getString s, e)

element :: Parser Json
element = brackets ws parseJson ws

data Json
  = JNull
  | JBoolean {getBoolean :: Bool}
  | JNumber {getNumber :: Double}
  | JString {getString :: String}
  | JArray {getArray :: [Json]}
  | JObject {getObject :: [(String, Json)]}
  deriving (Show)

parseJNull :: Parser Json
parseJNull = JNull <$ string "null"

parseJBoolean :: Parser Json
parseJBoolean = JBoolean <$> ((True <$ string "true") <|> (False <$ string "false"))

parseJNumber :: Parser Json
parseJNumber = JNumber <$> number

parseJString :: Parser Json
parseJString = JString <$> brackets (char '"') characters (char '"')

parseJArray :: Parser Json
parseJArray = JArray <$> brackets (char '[') (element `sepBy` char ',') (char ']')

parseJObject :: Parser Json
parseJObject = JObject <$> brackets (char '{') (member `sepBy` char ',') (char '}')

parseJson :: Parser Json
parseJson = parseJNull <|> parseJBoolean <|> parseJNumber <|> parseJString <|> parseJArray <|> parseJObject

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = do
  src <- prompt ">>> "
  print $ runParser parseJson src
