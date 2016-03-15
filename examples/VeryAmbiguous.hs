{-# LANGUAGE RecursiveDo #-}
import Control.Applicative
import System.Environment
import Text.Earley

g :: Grammar r (Prod r Char Char ())
g = mdo
  s <- rule $ () <$ token 'b'
           <|> () <$ s <* s
           <|> () <$ s <* s <* s
           <?> 's'
  return s

main :: IO ()
main = do
  xs:_ <- getArgs
  print $ report (parser g) xs
