module Lib
    where

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec
import Data.Functor.Identity
import Control.Applicative hiding ((<|>))
import qualified Text.Parsec.Token as P

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Op = Mov Register Register | Push Register
        deriving (Eq, Show)

data State = MyState

data Register = Register String | DereferencedRegister String | DereferencedRegisterOffset String Integer
  deriving (Eq, Show)

parseMov :: Parsec String () Op
parseMov = (asmReserved "mov") >> (Mov <$> parseRegisterLiteral <* (asmLexeme $ char ',') <*> parseRegisterLiteral)

parsePush :: Parsec String () Op
parsePush = asmReserved "push" >> Push <$> parseRegisterLiteral

parseRegister :: Parsec String () Register
parseRegister = parseRegisterLiteral <|> (try parseRegisterValueOffSet) <|> parseDereferencedRegister

parseRegisterLiteralToString :: ParsecT String () Identity String
parseRegisterLiteralToString = asmLexeme $ liftA3 mkRegister letter letter letter

parseRegisterLiteral :: ParsecT String () Identity Register
parseRegisterLiteral = Register <$> parseRegisterLiteralToString

parseDereferencedRegister :: Parsec String () Register
parseDereferencedRegister = asmBrackets $ DereferencedRegister <$> parseRegisterLiteralToString

parseRegisterValueOffSet :: ParsecT String () Identity Register
parseRegisterValueOffSet = asmBrackets (DereferencedRegisterOffset <$> parseRegisterLiteralToString <*> asmInteger)


parseDWordPtr :: Parsec String () Register
parseDWordPtr = asmReserved "DWORD" >> asmReserved "PTR" >> parseRegister
-- parseDWordPtr = undefined

mkRegister :: Char -> Char -> Char -> String
mkRegister a b c = [a, b, c]

tokenParser :: P.TokenParser ()
tokenParser = P.makeTokenParser asmIntelDef


asmLexeme = P.lexeme tokenParser
asmReserved = P.reserved tokenParser
asmBrackets = P.brackets tokenParser
asmInteger = P.integer tokenParser

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
    ,P.reservedNames = ["mov", "push", "DWORD", "PTR"]
    ,P.reservedOpNames = []
    ,P.caseSensitive = False
  }
  where none = satisfy $ const False