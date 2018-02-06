module Lib
    where

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec
import Data.Functor.Identity
import Control.Applicative
import qualified Text.Parsec.Token as P

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Op = Mov String String
        deriving (Eq, Show)

data State = MyState

parseMov :: Parsec String () Op
parseMov = (asmLexeme $ string "mov") >> (Mov <$> parseRegister <* (asmLexeme $ char ',') <*> parseRegister)

-- parsePush :: Parsec String () Op

parseRegister = asmLexeme $ liftA3 mkRegister letter letter letter

mkRegister :: Char -> Char -> Char -> String
mkRegister a b c = [a, b, c]

tokenParser :: P.TokenParser ()
tokenParser = P.makeTokenParser asmIntelDef


asmLexeme = P.lexeme tokenParser 

asmIntelDef :: P.GenLanguageDef String () Identity
asmIntelDef = P.LanguageDef 
  {
     P.commentStart = ""
    ,P.commentEnd = ""
    ,P.commentLine = "//"
    ,P.nestedComments = False
    ,P.identStart = letter
    ,P.identLetter = alphaNum
    ,P.opStart = none
    ,P.opLetter = none
    ,P.reservedNames = []
    ,P.reservedOpNames = []
    ,P.caseSensitive = False
  } 
  where none = satisfy $ const False