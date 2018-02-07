module AtParser where

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec
import Data.Functor.Identity
import Control.Applicative hiding ((<|>))
import qualified Text.Parsec.Token as P

data Op = Mov WordQualifier Register Register | Push WordQualifier Register
    deriving (Eq, Show)

data State = MyState

data WordQualifier = Q
  deriving (Eq, Show)

data Register = Register String | DereferencedRegister String | DereferencedRegisterOffset String Integer
  deriving (Eq, Show)

parseMov :: Parsec String () Op
parseMov = (Mov <$> (qualifiedInstruction "mov") <*> parseRegisterLiteral <* (asmLexeme $ char ',') <*> parseRegisterLiteral)

parsePush :: Parsec String () Op
parsePush = Push <$> qualifiedInstruction "push" <*> parseRegisterLiteral

qualifiedInstruction :: String -> Parsec String () WordQualifier
qualifiedInstruction name = asmLexeme $ const Q <$> (string name >> (char 'q'))

parseRegister :: Parsec String () Register
parseRegister = parseRegisterLiteral <|> (try parseRegisterValueOffSet) <|> parseDereferencedRegister

parseRegisterLiteralToString :: ParsecT String () Identity String
parseRegisterLiteralToString = asmLexeme $ char '%' >> liftA3 mkRegister letter letter letter

parseRegisterLiteral :: ParsecT String () Identity Register
parseRegisterLiteral = Register <$> parseRegisterLiteralToString

parseDereferencedRegister :: Parsec String () Register
parseDereferencedRegister = asmBrackets $ DereferencedRegister <$> parseRegisterLiteralToString

parseRegisterValueOffSet :: ParsecT String () Identity Register
parseRegisterValueOffSet = asmBrackets (DereferencedRegisterOffset <$> parseRegisterLiteralToString <*> asmInteger)

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
    ,P.reservedNames = ["mov", "push"]
    ,P.reservedOpNames = []
    ,P.caseSensitive = False
  }
  where none = satisfy $ const False