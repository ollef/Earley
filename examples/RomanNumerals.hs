{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Applicative ((<*), (<|>), some)
import System.Environment (getArgs)
import Text.Earley

data RomanSymbol = I | IV | V | IX | X | XL | L | XC | C | CD | D | CM | M
  deriving (Show, Read)

type RomanNumeral = [RomanSymbol]

symbolToDec :: RomanSymbol -> Int
symbolToDec I = 1
symbolToDec IV = 4
symbolToDec V = 5
symbolToDec IX = 9
symbolToDec X = 10
symbolToDec XL = 40
symbolToDec L = 50
symbolToDec XC = 90
symbolToDec C = 100
symbolToDec CD = 400
symbolToDec D = 500
symbolToDec CM = 900
symbolToDec M = 1000

romanNumeralToDec :: RomanNumeral -> Int
romanNumeralToDec = sum . map symbolToDec

type RomanNumeralProd r = Prod r RomanSymbol Char RomanNumeral

parseMany :: Int -> RomanSymbol -> RomanNumeralProd r
parseMany n s = replicate n s <$ (list $ concat $ replicate n (show s))

parseSingle :: RomanSymbol -> RomanNumeralProd r
parseSingle = parseMany 1

parseSome :: RomanSymbol -> RomanNumeralProd r
parseSome = fmap concat . some . parseSingle

andThen :: RomanNumeralProd r -> RomanNumeralProd r -> RomanNumeralProd r
andThen a b = fmap (++) a <*> b

andThenAny :: RomanNumeralProd r -> [RomanNumeralProd r] -> RomanNumeralProd r
andThenAny a = foldl1 (<|>) . map (a `andThen`)

romanNumeralsGrammar :: Grammar r (RomanNumeralProd r)
romanNumeralsGrammar = mdo
  
  i <- rule $ parseMany 3 I <|> parseMany 2 I <|> parseSingle I <?> I

  iv <- rule $ parseSingle IV <?> IV

  v <- rule $ parseSingle V <|> parseSingle V `andThen` i <?> V

  ix <- rule $ parseSingle IX <?> IX

  x' <- rule $ parseMany 3 X <|> parseMany 2 X <|> parseSingle X
  x <- rule $ x' <|> x' `andThenAny` [ix, v, iv, i] <?> X

  xl <- rule $ parseSingle XL <|> parseSingle XL `andThenAny` [ix, v, iv, i] <?> XL

  l <- rule $ parseSingle L <|> parseSingle L `andThenAny` [x, ix, v, iv, i] <?> L

  xc <- rule $ parseSingle XC <|> parseSingle XC `andThenAny` [ix, v, iv, i] <?> XC

  c' <- rule $ parseMany 3 C <|> parseMany 2 C <|> parseSingle C
  c <- rule $ c' <|> c' `andThenAny` [xc, l, xl, x, ix, v, iv, i] <?> C

  cd <- rule $ parseSingle CD <|> parseSingle CD `andThenAny` [xc, l, xl, x, ix, v, iv, i] <?> CD

  d <- rule $ parseSingle D <|> parseSingle D `andThenAny` [c, xc, l, xl, x, ix, v, iv, i] <?> D

  cm <- rule $ parseSingle CM <|> parseSingle CM `andThenAny` [xc, l, xl, x, ix, v, iv, i] <?> CM

  m' <- rule $ parseSome M
  m <- rule $ m' <|> m' `andThenAny` [cm, d, cd, c, xc, l, xl, x, ix, v, iv, i] <?> M

  s <- rule $ m <|> cm <|> d <|> cd <|> c <|> xc <|> l <|> xl <|> x <|> ix <|> v <|> iv <|> i

  return s

main :: IO ()
main = do
  x:_ <- getArgs
  print $ fullParses (parser romanNumeralsGrammar) x
