{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Applicative ((<|>), (<**>))
import System.Environment (getArgs)
import Text.Earley

numeral :: String -> Int -> Prod r String Char Int
numeral str n = n <$ list str

romanNumeralsGrammar :: Grammar r (Prod r String Char Int)
romanNumeralsGrammar = mdo

  thousands <- rule 
    $ pure 0 
    <|> numeral "M" 1000 <**> fmap (+) thousands

  le300 <- rule 
    $ pure 0 
    <|> numeral "C" 100 
    <|> numeral "CC" 200 
    <|> numeral "CCC" 300

  hundreds <- rule 
    $ le300 
    <|> numeral "CD" 400 
    <|> numeral "D" 500 <**> fmap (+) le300 
    <|> numeral "CM" 900

  le30 <- rule
    $ pure 0
    <|> numeral "X" 10
    <|> numeral "XX" 20
    <|> numeral "XXX" 30

  tens <- rule 
    $ le30
    <|> numeral "XL" 40
    <|> numeral "L" 50 <**> fmap (+) le30
    <|> numeral "XC" 90

  le3 <- rule
    $ pure 0
    <|> numeral "I" 1
    <|> numeral "II" 2
    <|> numeral "III" 3

  units <- rule
    $ le3
    <|> numeral "IV" 4
    <|> numeral "V" 5 <**> fmap (+) le3
    <|> numeral "IX" 9

  return
    $ thousands 
    <**> fmap (+) hundreds 
    <**> fmap (+) tens 
    <**> fmap (+) units
  

main :: IO ()
main = do
  x:_ <- getArgs
  print $ fullParses (parser romanNumeralsGrammar) x
