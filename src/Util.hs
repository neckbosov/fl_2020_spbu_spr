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

parseBracketize :: Parser String String output -> Parser String String output
parseBracketize p = symbol '(' *> parseWss *> p <* parseWss <* symbol ')'

packPair :: Functor f => (a, f b) -> f (a,b)
packPair (x, y) = (\z -> (x, z)) <$> y
