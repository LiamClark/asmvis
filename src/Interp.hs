module Interp where

import AtParser

data InterpState = InterpState {
    instructionIndex :: Int,
    registers :: RegisterState,
    stack :: [StackFrame]
} deriving (Eq, Show)

startState = InterpState 0 (RegisterType 0) [(StackFrame 0)]

newtype RegisterState = RegisterType Int deriving (Eq, Show)
newtype StackFrame = StackFrame Int deriving (Eq, Show)

stepBody :: Body -> InterpState -> InterpState
stepBody body state = startState