{-# LANGUAGE RecursiveDo #-}
import Control.Applicative
import Text.Earley
import qualified Data.HashSet as HS

type Noun      = String
type Verb      = String
type Adjective = String

nouns, verbs, adjectives :: HS.HashSet String
nouns      = HS.fromList ["parsers", "sentences", "grammars"]
verbs      = HS.fromList ["parse", "munch", "annihilate", "confuse", "use"]
adjectives = HS.fromList ["many", "great", "long", "confusing"]


data Sentence = Sentence NounPhrase VerbPhrase
  deriving Show
data NounPhrase = NounPhrase Adjective NounPhrase
                | Noun Noun
  deriving Show
data VerbPhrase = VerbPhrase Verb NounPhrase
                | Verb Verb
  deriving Show

sentence :: Grammar r String (Prod r String String Sentence)
sentence = mdo
  noun       <- rule $ satisfy (`HS.member` nouns)      <?> "noun"
  verb       <- rule $ satisfy (`HS.member` verbs)      <?> "verb"
  adjective  <- rule $ satisfy (`HS.member` adjectives) <?> "adjective"
  nounPhrase <- rule $  NounPhrase <$> adjective <*> nounPhrase
                    <|> Noun <$> noun
                    <?> "noun phrase"
  verbPhrase <- rule $  VerbPhrase <$> verb <*> nounPhrase
                    <|> Verb <$> verb
                    <?> "verb phrase"
  return $ Sentence <$> nounPhrase <*> verbPhrase <?> "sentence"

main :: IO ()
main = do
  let p = fullParses (parser sentence) . words
  print $ p "parsers use grammars"
  print $ p "parsers munch long sentences"
  print $ p "many great sentences confuse parsers"
  print $ p "parsers use use"
  print $ p "grammars many great confusing"
