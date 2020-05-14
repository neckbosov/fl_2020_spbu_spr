module Regexp where

import           Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative _ Empty    = Empty
derivative _ Epsilon  = Empty
derivative c (Char a) = if c == a then Epsilon else Empty
derivative c (Seq r s) = Alt r1 r2
    where
        r1 = Seq (derivative c r) s
        r2 = if nullable r then derivative c s else Empty
derivative c (Alt r s) = Alt (derivative c r) (derivative c s)
derivative c (Star r) = Seq (derivative c r) (Star r)

nullable :: Regexp -> Bool
nullable Empty     = False
nullable Epsilon   = True
nullable (Char _)  = False
nullable (Alt r s) = nullable r || nullable s
nullable (Seq r s) = nullable r && nullable s
nullable (Star r)  = True
