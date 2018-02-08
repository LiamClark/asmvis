import Test.Hspec
import Text.Parsec
import AtParserSpec
import InterpSpec

main :: IO ()
main = hspec $ do 
    atSpec
    interpSpec