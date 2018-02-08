module InterpSpec (
    interpSpec
) where

import Text.Parsec
import Test.Hspec
import AtParserSpec
import AtParser
import Interp
import Data.Either
import qualified Data.Map.Strict as Map

interpSpec :: Spec
interpSpec = do
        integrationSpec
        unitSpec


integrationSpec :: Spec
integrationSpec =
    describe "A naive interpreter" $
        it "can do stuffies" $
         stepBody body startState `shouldBe` (Right startState)
    where   
        body = parseSucceeds entireFileBody


unitSpec :: Spec
unitSpec = 
    describe "A naive interpreter" $ do
        it "can intepret a move from register to register" $
          applyOp op initialState `shouldBe` (Right expected)
        registerSpec
        dereferenceRegisterSpec
    where 
        op :: Op
        op = parseSucceeds (parse parseMov "hi.asm" "movq %rsp, %rbp") 
        initialState = InterpState 0 (RegisterState 1 0 0 0) Map.empty
        expected     = InterpState 0 (RegisterState 1 1 0 0) Map.empty

dereferenceRegisterSpec :: Spec
dereferenceRegisterSpec = it "can intepret a move from a dereferenced register to a register" $
        applyOp op initialState `shouldBe` (Right expected)
    where
        op = parseSucceeds (parse parseMov "hi.asm" "movq (%rsp), %rbp") 
        initialState = InterpState 0 (RegisterState 1 0 0 0) (Map.singleton 1 5)
        expected     = InterpState 0 (RegisterState 1 5 0 0) (Map.singleton 1 5)

registerSpec :: Spec
registerSpec = 
    describe "Register state" $
        it "storing 2 in rbp should give 0 2 0 0" $ 
            storeInRegister (Register "rbp") regState 2 `shouldBe` (Right expected)
    where regState = (RegisterState 0 1 0 0)
          expected = (RegisterState 0 2 0 0)

parseSucceeds :: Show a => Either a b -> b
parseSucceeds (Left err) = error . show $ err
parseSucceeds (Right x) = x