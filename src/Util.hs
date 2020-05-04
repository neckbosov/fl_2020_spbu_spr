module Util where

import           Combinators         (InputStream (..), Parser (..),
                                      Position (..), Result (..), incrCol,
                                      incrLine, makeError, satisfy, success,
                                      symbol, word)

import           Control.Applicative
import           Control.Monad
import           Data.Char           (isSpace)

parseTab :: Parser String String Char
parseTab = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | x == '\t' -> Success (InputStream xs pos {col = ((col pos + 4) `div` 4) * 4}) x
    input              -> Failure [makeError "Predicate failed" pos]

parseLineBreak :: Parser String String Char
parseLineBreak = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | x == '\n' -> Success (InputStream xs (Position (line pos + 1) 0)) x
    input              -> Failure [makeError "Predicate failed" pos]

parseSpace :: Parser String String Char
parseSpace = symbol ' '

parseWs :: Parser String String Char
parseWs = parseSpace <|> parseTab <|> parseLineBreak

parseSpaces :: Parser String String String
parseSpaces = many parseSpace

parseWss :: Parser String String String
parseWss = many parseWs

bracketize :: Parser String String output -> Parser String String output
bracketize p = symbol '(' *> parseWss *> p <* parseWss <* symbol ')'

spaceSurrounded p = parseSpaces *> p <* parseSpaces

wsSurrounded p = parseWss *> p <* parseWss
