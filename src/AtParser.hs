module AtParser where 

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec
import Data.Functor.Identity
import Control.Applicative hiding ((<|>))
import qualified Text.Parsec.Token as P

data Body = Body [Op]
  deriving (Eq, Show)

data Op = Mov WordQualifier Register Register 
        | Push WordQualifier Register
        | IMul WordQualifier Register Register
        | Pop WordQualifier Register
        | Ret
    deriving (Eq, Show)

data State = MyState

data WordQualifier = Q | L
  deriving (Eq, Show)

data Register = Register String | DereferencedRegister String | DereferencedRegisterOffset Integer String
  deriving (Eq, Show)

parseBody :: Parsec String () Body
parseBody = Body <$> (many1 . choice)  (try <$> lineParsers)
    where lineParsers = [parseMov, parsePush, parseIMul, parsePop, parseRet]

parseMov :: Parsec String () Op
parseMov = parseTwoRegisters $ Mov <$> (qualifiedInstruction "mov")

parsePush :: Parsec String () Op
parsePush = Push <$> qualifiedInstruction "push" <*> parseRegister

parseIMul :: Parsec String () Op
parseIMul = parseTwoRegisters $ IMul <$> qualifiedInstruction "imul"

parsePop :: Parsec String () Op
parsePop = Pop <$> qualifiedInstruction "pop" <*> parseRegister

parseRet :: Parsec String () Op
parseRet = const Ret <$> asmReserved "ret"

qualifiedInstruction :: String -> Parsec String () WordQualifier
qualifiedInstruction name = asmLexeme $ (string name >> parseWordQualifier)

parseWordQualifier :: Parsec String () WordQualifier
parseWordQualifier =  partialConvert <$> oneOf "lq"
    where partialConvert x = case x of
                                'l' -> L
                                'q' -> Q

parseTwoRegisters :: Parsec String () (Register -> Register -> Op)  -> Parsec String () Op
parseTwoRegisters op = op <*> parseRegister <* (asmLexeme $ char ',') <*> parseRegister

parseRegister :: Parsec String () Register
parseRegister = (try parseDereferencedRegister) <|> (try parseRegisterLiteral) <|> (try parseRegisterValueOffSet)

parseRegisterLiteral :: ParsecT String () Identity Register
parseRegisterLiteral = Register <$> parseRegisterLiteralToString

parseDereferencedRegister :: Parsec String () Register
parseDereferencedRegister = asmParens $ DereferencedRegister <$> parseRegisterLiteralToString

parseRegisterValueOffSet :: ParsecT String () Identity Register
parseRegisterValueOffSet =  DereferencedRegisterOffset <$> asmInteger <*> asmParens parseRegisterLiteralToString

parseRegisterLiteralToString :: ParsecT String () Identity String
parseRegisterLiteralToString = asmLexeme $ char '%' >> liftA3 mkRegister letter letter letter

mkRegister :: Char -> Char -> Char -> String
mkRegister a b c = [a, b, c]

tokenParser :: P.TokenParser ()
tokenParser = P.makeTokenParser asmIntelDef


asmLexeme = P.lexeme tokenParser
asmReserved = P.reserved tokenParser
asmBrackets = P.brackets tokenParser
asmInteger = P.integer tokenParser
asmParens = P.parens tokenParser

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