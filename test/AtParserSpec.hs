module AtParserSpec (
  atSpec,
  entireFileBody
) where

import Test.Hspec
import Text.Parsec
import AtParser
import Data.Either

atSpec :: Spec
atSpec = do
    atUnitSpec
    atIntegrationSpec


atIntegrationSpec :: Spec
atIntegrationSpec = describe "the at&t parser" $
        it "can parse the whole file" $
           entireFileBody `shouldSatisfy` isRight


entireFileBody :: Either ParseError Body
entireFileBody = (parse parseBody name file)
  where
    file = "pushq %rbp \n\
    \movq %rsp, %rbp\n\
    \movl %edi, -4(%rbp)\n\
    \movl -4(%rbp), %eax\n\
    \imull -4(%rbp), %eax\n\
    \popq %rbp\n\
    \ret\n"
    name = "hi.asm"

atUnitSpec ::Spec
atUnitSpec = describe "an at&t syntax parser" $ do
        it "can parse mov" $
          (parse parseMov name "movq %rsp, %rbp")  `shouldBe` (Right $ Mov Q rsp rbp)
        it "can parse push" $
          (parse parsePush name "pushq %rbp") `shouldBe` (Right $ Push Q rbp)
        it "can parse mov with dereference offsets" $
          (parse parseMov name "movl %edi, -4(%rbp)") `shouldBe` (Right $ Mov L edi belowBase )
        it "can parse imull" $
          (parse parseIMul name "imull -4(%rbp), %eax") `shouldBe` (Right $ IMul L belowBase eax)
        it "can parse pop" $
          (parse parsePop name "popq %rbp") `shouldBe` (Right $ Pop Q rbp)
        it "can parse ret" $
          (parse parseRet name "ret") `shouldBe` (Right $ Ret)
    where
        name = "hi.asm"
        rbp = Register "rbp"
        rsp = Register "rsp"
        edi = Register "edi"
        eax = Register "eax"
        belowBase = DereferencedRegisterOffset (-4) "rbp"
