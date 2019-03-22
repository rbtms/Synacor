module DataTypes where

import qualified Data.Vector.Unboxed as V

data Error =
    ERR_PTR_OOB
  | ERR_MEM_INDEX_OOB
  | ERR_EMPTY_STACK
  | ERR_NOT_ASCII
  | ERR_INVALID_OPCODE
  | ERR_INVALID_REGISTER
  deriving Show

data Arg = Val Int | Reg Int | Int' Int | Null
  deriving Eq

instance Show Arg where
  show (Val  n) = show n
  show (Reg  n) = "REG" ++ show n
  show (Int' n) = show n
  show Null     = "Null"

data Opcode =
    Halt
  | Set   Arg Arg
  | Push  Arg
  | Pop   Arg
  | Eq    Arg Arg Arg
  | Gt    Arg Arg Arg
  | Jmp   Arg
  | Jt    Arg Arg
  | Jf    Arg Arg
  | Add   Arg Arg Arg
  | Mult  Arg Arg Arg
  | Mod   Arg Arg Arg
  | And   Arg Arg Arg
  | Or    Arg Arg Arg
  | Not   Arg Arg
  | Rmem  Arg Arg
  | Wmem  Arg Arg
  | Call  Arg
  | Ret
  | Out   Arg
  | In    Arg
  | Noop
  | Trash
  deriving Eq -- (Eq, Show)

instance Show Opcode where
  show Halt         = "HALT"
  show (Set a b)    = "SET   " ++ show a ++ " <- " ++ show b
  show (Push a)     = "PUSH  " ++ show a ++ " into stack"
  show (Pop a)      = "POP   " ++ "stack into "  ++ show a
  show (Eq a b c)   = "EQ    " ++ show a ++ " = " ++ show b ++ " == " ++ show c
  show (Gt a b c)   = "GT    " ++ show a ++ " = " ++ show b ++ " > "  ++ show c
  show (Jmp a)      = "JMP   " ++ show a ++ "\n"
  show (Jt a b)     = "JMP   " ++ show b ++ " if " ++ show a ++ " /= 0\n"
  show (Jf a b)     = "JMP   " ++ show b ++ " if " ++ show a ++ " == 0\n"
  show (Add a b c)  = "ADD   " ++ show a ++ " = " ++ show b ++ " + "   ++ show c
  show (Mult a b c) = "MULT  " ++ show a ++ " = " ++ show b ++ " * "   ++ show c
  show (Mod a b c)  = "MOD   " ++ show a ++ " = " ++ show b ++ " mod " ++ show c
  show (And a b c)  = "AND   " ++ show a ++ " = " ++ show b ++ " and " ++ show c
  show (Or a b c)   = "OR    " ++ show a ++ " = " ++ show b ++ " or "  ++ show c
  show (Not a b)    = "NOT   " ++ show a ++ " = " ++ "not " ++ show b
  show (Rmem a b)   = "RMEM  " ++ show a ++ " = " ++ "MEM[" ++ show b ++ "]"
  show (Wmem a b)   = "WMEM  " ++ "MEM[" ++ show a ++ "] = " ++ show b
  show (Call a)     = "CALL  " ++ show a ++ "\n"
  show Ret          = "RET\n"
  show (Out a)      = "OUT   " ++ show a
  show (In a)       = "IN    " ++ show a
  show Noop         = "NOOP  "
  show Trash        = "TRASH"

data State = State {
  -- | Memory
    _mem       :: V.Vector Int
  -- | Registers
  , _reg       :: V.Vector Int
  -- | Stack
  , _stack     :: [Int]
  -- | Instruction pointer
  , _ptr       :: Int
  -- | Call trace
  , _trace     :: [Int]
  -- | Pointer trace
  , _ptr_trace :: [Int]
  -- | Character input stream
  , _input     :: [Int]
  -- | Debug flag
  , _is_debug  :: Bool
  -- | Halt flag
  , _is_halt   :: Bool
}


instance Show State where
  show (State mem reg stack ptr trace ptr_trace input is_debug is_halt) =
         "REG       | " ++ show reg
    ++ "\nSTACK     | " ++ (show . take 50 $ stack)
    ++ "\nPTR       | " ++ show ptr
    ++ "\nINPUT     | " ++ show input
    ++ "\nDEBUG     | " ++ show is_debug
    ++ "\nHALT      | " ++ show is_halt
    ++ "\nTRACE     | " ++ (show . take 50 $ trace)
    ++ "\nPTR_TRACE | " ++ (show . take 50 $ ptr_trace)

