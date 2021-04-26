module Parsing.Json.Prettify
    ( Doc
    , char
    , compact
    , double
    , pretty
    , string
    , series
    , text
    ) where

import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import Data.Foldable (fold)
import Numeric (showHex)

data Doc
    = Empty
    | Char Char
    | Text String
    | Line
    | Concat Doc Doc
    | Union Doc Doc 
  deriving (Eq, Show)

instance Semigroup Doc where
    Empty <> d = d
    d <> Empty = d
    d1 <> d2   = Concat d1 d2

instance Monoid Doc where
    mempty = Empty

empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

double :: Double -> Doc
double = Text . show

string :: String -> Doc
string = enclose '"' '"' . fold . map oneChar

text :: String -> Doc
text "" = Empty
text s  = Text s

line :: Doc
line = Line

compact :: Doc -> String
compact a = transform [a]
  where
    transform [] = ""
    transform (d:ds) =
        case d of
            Empty -> transform ds
            Char c -> c : transform ds
            Text s -> s ++ transform ds
            Line -> '\n' : transform ds
            Concat d1 d2 -> transform (d1:d2:ds)
            Union _ d' -> transform (d':ds)            

pretty :: Int -> Doc -> String
pretty width a = best 0 [a]
  where
    best col (d:ds) =
        case d of
            Empty -> best col ds
            Char c -> c : best (col + 1) ds
            Text s -> s ++ best (col + length s) ds
            Line -> '\n' : best 0 ds
            Concat d1 d2 -> best col (d1:d2:ds)
            Union d1 d2 -> nicest col (best col (d1:ds)) (best col (d2:ds))
    best _ _ = ""
    
    nicest col d1 d2
        | (width - least) `fits` d1 = d1
        | otherwise = d2
      where
        least = min width col

smallHex :: Int -> Doc
smallHex x = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
  where
    h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c
    | d < 0x10000 = smallHex d
    | otherwise   = astral (d - 0x10000)
  where
    d = ord c

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
    Just r -> text r
    Nothing | mustEscape c -> hexEscape c
            | otherwise    -> char c
  where
    mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where
    ch a b = (a, ['\\', b])

enclose :: Char -> Char -> Doc -> Doc
enclose pre suf value = char pre <> value <> char suf

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series pre suf f = enclose pre suf . fsep . punctuate (char ',') . map f

fsep :: [Doc] -> Doc
fsep = foldr (</>) Empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <> b

softline :: Doc
softline = group line

group :: Doc -> Doc
group a = flatten a `Union` a

flatten :: Doc -> Doc
flatten (Concat a b) = Concat (flatten a) (flatten b)
flatten Line         = Char ' '
flatten (Union a _)  = flatten a
flatten other        = other

fits :: Int -> String -> Bool
fits w _ | w < 0 = False
fits _ ""        = True
fits _ ('\n':_)  = True
fits w (c:cs)    = (w - 1) `fits` cs
