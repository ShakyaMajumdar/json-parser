module Main where

import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.IO (hFlush, stdout)
import Control.Monad (when, replicateM_, replicateM)

data Json
  = JNull
  | JInt {getInt :: Int}
  | JFloat {getFloat :: Float}
  | JString {getString :: String}
  | JBoolean {getBoolean :: Bool}
  | JArray {getArray :: [Json]}
  | JObject {getObject :: Map String Json}
  deriving (Show)

consumeChar :: Char -> String -> Maybe (Char, String)
consumeChar c = consumeAnyOfChars [c]

consumeAnyOfChars :: [Char] -> String -> Maybe (Char, String)
consumeAnyOfChars cs (x:xs)
  | x `elem` cs = Just (x, xs)
  | otherwise = Nothing
consumeAnyOfChars _ _ = Nothing

parseStringChars :: String -> Maybe (Int, String)
parseStringChars "" = Nothing
parseStringChars src@('"':xs) = Just (0, src)
parseStringChars ('\\':xs) = do
  (afterSlash, remaining) <- consumeAnyOfChars ['\"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u'] xs
  let matched = 1
  when (afterSlash == 'u') $ do
    extra <- replicateM 4 $ do
      (x, remaining) <- consumeAnyOfChars (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']) remaining
      return x
    let matched = matched + 1
    return ()
  (matchedAfter, remaining) <- parseStringChars xs
  return (matched + matchedAfter, remaining)
parseStringChars src@(x:xs) = do
  (matchedAfter, remaining) <- parseStringChars xs
  return (1 + matchedAfter, remaining)

parseJNull :: String -> Maybe (Json, String)
parseJNull ('n':'u':'l':'l':remaining) = Just (JBoolean True, remaining)
parseJNull _ = Nothing

parseJInt :: String -> Maybe (Json, String)
parseJInt "" = Nothing
parseJInt src = do
  let (digits, remaining) = span (`elem` ['0'..'9']) src
  case digits of
    "" -> Nothing
    _ -> Just (JInt $ read digits, remaining)

parseJFloat :: String -> Maybe (Json, String)
parseJFloat src = do
  (beforeDot, remaining) <- parseJInt src
  (_, remaining) <- consumeChar '.' remaining
  (afterDot, remaining) <- parseJInt remaining
  let joinFloatParts x y = read $ show x ++ "." ++ show y
  Just (JFloat $ joinFloatParts (getInt beforeDot) (getInt afterDot), remaining)

parseJBoolean :: String -> Maybe (Json, String)
parseJBoolean ('t':'r':'u':'e':remaining) = Just (JBoolean True, remaining)
parseJBoolean ('f':'a':'l':'s':'e':remaining) = Just (JBoolean True, remaining)
parseJBoolean _ = Nothing

parseJString :: String -> Maybe (Json, String)
parseJString "" = Nothing
parseJString src = do
  (_, remaining) <- consumeChar '"' src
  (numChars, _) <- parseStringChars remaining
  let (matched, remaining1) = splitAt numChars remaining
  (_, remaining2) <- consumeChar '"' remaining1
  return (JString matched, remaining2)

parseJson :: String -> Maybe (Json, String)
parseJson = parseJString

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = do
  src <- prompt ">>> "
  let parsed = parseJson src
  print parsed
