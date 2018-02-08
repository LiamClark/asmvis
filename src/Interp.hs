module Interp where

import AtParser

data InterpState = InterpState {
    instructionIndex :: Int,
    registers :: RegisterState,
    stack :: [StackFrame]
} deriving (Eq, Show)

startState = InterpState 0 (RegisterState 0 0 0 0) [(StackFrame 0)]

data RegisterState = RegisterState {
    rsp :: Int,
    rbp :: Int,
    edi :: Int,
    eax :: Int
} deriving (Eq, Show)

newtype StackFrame = StackFrame Int deriving (Eq, Show)

data InterpError = NoSuchRegister String deriving (Eq, Show)

stepBody :: Body -> InterpState -> Either InterpError InterpState
stepBody body (InterpState index registers stack) = Right startState
    where op = ops body !! index

applyOp :: Op -> InterpState -> Either InterpError InterpState
applyOp (Mov word source dest) state = (\x -> state {registers = x}) <$> updatedState
    where value = registerValue (registers state) source
          updatedState :: Either InterpError RegisterState
          updatedState =  value >>= storeInRegister dest (registers state)

storeInRegister :: Register -> RegisterState -> Int -> Either InterpError RegisterState
storeInRegister (Register name) state = simpleRegisterStore state name

simpleRegisterStore :: RegisterState -> String -> Int -> Either InterpError RegisterState
simpleRegisterStore registers "rsp" value = Right $ registers {rsp = value}
simpleRegisterStore registers "rbp" value = Right $ registers {rbp = value}
simpleRegisterStore registers "edi" value = Right $ registers {edi = value}
simpleRegisterStore registers "eax" value = Right $ registers {eax = value}
simpleRegisterStore registers reg value   = Left  $ NoSuchRegister reg

registerValue :: RegisterState -> Register -> Either InterpError Int
registerValue state (Register name)             = getRegister name state
-- registerValue state (DereferencedRegister name) = 
-- registerValue state (DereferencedRegisterOffset Integer String) = 

getRegister :: String -> RegisterState -> Either InterpError Int
getRegister "rsp" = Right . rsp
getRegister "rbp" = Right . rbp
getRegister "edi" = Right . edi
getRegister "eax" = Right . eax
getRegister  reg  = const $ Left (NoSuchRegister reg)