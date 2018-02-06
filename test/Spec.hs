import Test.Hspec
-- import Test.QuickCheck
import Text.Parsec
import Lib 

main :: IO ()
main = hspec $ do
    describe "Parse mov" $ do
        it "parses mov" $ (parse parseMov name "mov rbp, rsp")  `shouldBe` (Right $ Mov rbp rsp)
        it "parses push" $ (parse parsePush name "push rbp") `shouldBe` (Right $ Push rbp)
    where 
        name = "hi.asm"
        rbp = Register "rbp"
        rsp = Register "rsp"

