module Interp where

import AtParser
import qualified Data.Map.Strict as Map

data InterpState = InterpState {
    instructionIndex :: Int,
    registers :: RegisterState,
    stack :: Map.Map Int Int
} deriving (Eq, Show)

startState = InterpState 0 (RegisterState 0 0 0 0) Map.empty

data RegisterState = RegisterState {
    rsp :: Int,
    rbp :: Int,
    edi :: Int,
    eax :: Int
} deriving (Eq, Show)
 
data InterpError = NoSuchRegister String 
    | NoSuchMemoryLocation Int
    deriving (Eq, Show)

stepBody :: Body -> InterpState -> Either InterpError InterpState
stepBody body (InterpState index registers stack) = Right startState
    where op = ops body !! index

applyOp :: Op -> InterpState -> Either InterpError InterpState
applyOp (Mov word source dest) state = (\x -> state {registers = x}) <$> updatedState
    where value = registerValue state source
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

registerValue :: InterpState -> Register -> Either InterpError Int
registerValue state (Register name)             = getRegister name (registers state)
registerValue state (DereferencedRegister name) = getRegister name (registers state) >>= adress state
registerValue state (DereferencedRegisterOffset offset name) = getRegister name (registers state) >>= dereferenceWithOffSet state offset

dereferenceWithOffSet :: InterpState -> Integer -> Int -> Either InterpError Int
dereferenceWithOffSet state offset loc = adress state (loc + intOffSet)
    where 
        intOffSet :: Int
        intOffSet = fromInteger offset

adress :: InterpState -> Int -> Either InterpError Int
adress state address = note (NoSuchMemoryLocation address) $ Map.lookup address memory
    where memory = stack state

note :: a -> Maybe b -> Either a b
note x (Just b) = Right b
note x Nothing = Left x


getRegister :: String -> RegisterState -> Either InterpError Int
getRegister "rsp" = Right . rsp
getRegister "rbp" = Right . rbp
getRegister "edi" = Right . edi
getRegister "eax" = Right . eax
getRegister  reg  = const $ Left (NoSuchRegister reg)