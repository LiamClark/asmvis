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
         ($ startState) <$> step `shouldSatisfy` isRight
    where
        step :: Either ParseError (InterpState -> InterpState)
        step = stepBody <$> entireFileBody


