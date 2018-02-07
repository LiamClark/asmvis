module AtParserSpec where

import Test.Hspec
import Text.Parsec
import AtParser

atSpec ::Spec
atSpec =
    -- describe "registers" $ do
    --     it "can parse a register dereference offset" $
    --         (parse parseRegister name "[rbp-4]") `shouldBe` (Right $ DereferencedRegisterOffset "rbp" (-4))
    --     it "can parse a simple register" $
    --         (parse parseRegister name "rbp") `shouldBe` (Right $ Register "rbp")
    --     it "can parse a dereferenced register" $
    --         (parse parseRegister name "[rbp]") `shouldBe` (Right $ DereferencedRegister "rbp")
    describe "Parse mov" $ do
        it "parses mov" $ 
          (parse parseMov name "movq %rsp, %rbp")  `shouldBe` (Right $ Mov Q rsp rbp)
        it "parses push" $ 
          (parse parsePush name "pushq %rbp") `shouldBe` (Right $ Push Q rbp)
        it "parser mov with dereference offsets" $
          (parse parseMov name "movl %edi, -4(%rbp)") `shouldBe` (Right $ Mov L edi belowBase )
    where 
        name = "hi.asm"
        rbp = Register "rbp"
        rsp = Register "rsp"
        edi = Register "edi"
        belowBase = DereferencedRegisterOffset (-4) "rbp"
