{-# LANGUAGE RecursiveDo #-}
import Control.Applicative
import System.Environment
import Text.Earley

g :: Grammar r Char (Prod r Char Char ())
g = mdo
  s <- rule $ () <$ symbol 'b'
           <|> () <$ s <* s
           <|> () <$ s <* s <* s
           <?> 's'
  return s

main :: IO ()
main = do
  xs:_ <- getArgs
  print $ fullRecognitions $ parser g xs
