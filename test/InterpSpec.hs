module InterpSpec (
    interpSpec
) where

import Text.Parsec
import Test.Hspec
import AtParserSpec
import AtParser
import Interp
import Data.Either

interpSpec :: Spec
interpSpec =
    describe "A naive interpreter" $
        it "can do stuffies" $
         stepBody body startState `shouldBe` startState
    where
        body = parseSucceeds entireFileBody

parseSucceeds :: Show a => Either a b -> b
parseSucceeds (Left err) = error . show $ err
parseSucceds (Right x) = x