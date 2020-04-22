module Util where

import           Combinators         (Parser (..), satisfy, success, symbol,
                                      word)

import           Control.Applicative
import           Control.Monad
import           Data.Char           (isSpace)

parseSpace :: Parser String String Char
parseSpace = symbol ' '

parseWs :: Parser String String Char
parseWs = satisfy isSpace

parseSpaces :: Parser String String String
parseSpaces = many parseSpace

parseWss :: Parser String String String
parseWss = many parseWs

bracketize :: Parser String String output -> Parser String String output
bracketize p = symbol '(' *> parseWss *> p <* parseWss <* symbol ')'

spaceSurrounded p = parseSpaces *> p <* parseSpaces

wsSurrounded p = parseWss *> p <* parseWss
