import Test.Hspec
import Text.Parsec
import Lib 

main :: IO ()
main = hspec $ do
    describe "registers" $ do
        it "can parse a register dereference offset" $
            (parse parseRegister name "[rbp-4]") `shouldBe` (Right $ DereferencedRegisterOffset "rbp" (-4))
        it "can parse a simple register" $
            (parse parseRegister name "rbp") `shouldBe` (Right $ Register "rbp")
        it "can parse a dereferenced register" $
            (parse parseRegister name "[rbp]") `shouldBe` (Right $ DereferencedRegister "rbp")

    describe "Parse mov" $ do
        it "parses mov" $ (parse parseMov name "mov rbp, rsp")  `shouldBe` (Right $ Mov rbp rsp)
        it "parses push" $ (parse parsePush name "push rbp") `shouldBe` (Right $ Push rbp)
    where 
        name = "hi.asm"
        rbp = Register "rbp"
        rsp = Register "rsp"

