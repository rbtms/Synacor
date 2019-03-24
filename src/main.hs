module Main where

import Data.Char (ord, chr, isAscii)
import Data.Bits ((.&.), (.|.), complement, clearBit)
import qualified Helper              as H
import qualified Data.Vector.Unboxed as V
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Debug.Trace (traceShow)
import DataTypes

-- | Throw an error
throw_err :: Error -> a
throw_err err = error $ show err ++ "\n"

-- | Throw an error with an argument
throw_err_arg :: (Show b) => Error -> b -> a
throw_err_arg err arg = error $ show err ++ " " ++ show arg

-- |
-- | Utility functions
-- |

---- | Registers

-- | Is register
is_reg :: Int -> Bool
is_reg n = n > 32767 && n < 32776

-- | Convert a register into a valid index
from_reg :: Int -> Int
from_reg n
  | not . is_reg $ n = throw_err_arg ERR_INVALID_REGISTER n
  | otherwise        = n - 32768

-- | Convert a valid index into a register
to_reg :: Int -> Int
to_reg n
  | not . is_reg $ n+32768 = throw_err_arg ERR_INVALID_REGISTER n
  | otherwise              = n + 32768

-- | Resolve a value to either a register or a raw value
get_val :: State -> Int -> Int
get_val s n
  | is_reg n  = _reg s V.! from_reg n
  | otherwise = n

-- | Set register r to val
set_reg :: State -> Int -> Int -> State
set_reg s r val = s { _reg = V.update (_reg s) $ V.fromList [(i, val)] }
  where i = from_reg r

---- | Stack

-- | Push a value into the stack
push_stack :: State -> Int -> State
push_stack s n = s { _stack = n : _stack s }

-- | Pop last value from the stack
pop_stack :: State -> Int -> State
pop_stack s r
  | null stack = throw_err ERR_EMPTY_STACK
  | otherwise  = ( set_reg s r val ) { _stack = s' }
  where stack    = _stack s
        (val:s') = stack

---- | Pointer

-- | Increase instruction pointer
inc_ptr :: Int -> State -> State
inc_ptr n s = s { _ptr = _ptr s + n }

-- | Decrease instruction pointer
dec_ptr :: Int -> State -> State
dec_ptr n s = s { _ptr = _ptr s - n }

-- | Set instruction pointer
set_ptr :: Int -> State -> State
set_ptr n s = s { _ptr = n }

---- | Memory

-- | Get mem!!i
get_mem :: State -> Int -> Int
get_mem s i
  | i < 0 || i >= V.length (_mem s) = throw_err_arg ERR_MEM_INDEX_OOB i
  | otherwise = _mem s V.! i

-- | Set mem s!!i to val
set_mem :: State -> Int -> Int -> State
set_mem s i val
  | i < 0 || i >= V.length (_mem s) = throw_err_arg ERR_MEM_INDEX_OOB i
  | otherwise = s { _mem = V.update (_mem s) $ V.fromList [(i, val)] }

---- | I/O

-- | Put a character
put_chr :: Int -> IO ()
put_chr n = putStr [chr n]

-- | Input "look" into the VM
prompt_look :: State -> State
prompt_look s = s { _input = map ord "look\n" }

-- | Command handler for _in
handle_cmd :: State -> [Char] -> IO State
handle_cmd s line
  -- | Quit VM
  | line == "q" = return $ _halt s
  -- | Save VM state
  | head line' == "save" = if length line' == 1
    then handle_cmd s "No filename"
    else do
      H.save s $ last line'
      return $ prompt_look s
  -- | Load VM state
  | head line' == "load" = if length line' == 1
    then handle_cmd s "No filename"
    else do
      s' <- H.load $ last line'
      return $ prompt_look s'
  -- | Set register
  | head line' == "setreg" = if length line' < 3
    then handle_cmd s "Not enough arguments"
    else do
      let [r, val] = [ H.to_int $ line'!!1, H.to_int $ line'!!2 ]
      return $ set_reg s (to_reg r) val
  | head line' == "state" = do
    putStrLn $ show s
    return $ prompt_look s
  -- | Decompile
  | head line' == "decompile" = if length line' == 1
    then handle_cmd s "No filename"
    else do
      H.log_asm s $ "../asm/" ++ last line' ++ ".txt"
      putStr "\nDecompile: Done!"
      -- | Prompt again
      return $ prompt_look s
  -- | Invalid command
  | otherwise = do
    putStrLn $ "Error: Invalid command (" ++ line ++ ")"
    return s
  where line' = words line

-- |
-- | Operators
-- |

_halt, _ret                            :: State -> State
_push, _pop, _jmp, _call               :: State -> Int -> State
_set, _jt, _jf, _not, _rmem, _wmem     :: State -> Int -> Int -> State
_eq, _gt, _add, _mult, _mod, _and, _or :: State -> Int -> Int -> Int -> State
_out                                   :: Int -> IO ()
_in                                    :: State -> Int -> IO State

-- | Halt VM
_halt s = s { _is_halt = True }

-- | Set register a to <b>
_set = set_reg

-- | Push <a> into stack
_push = push_stack
-- | Pop stack and save in register a
_pop = pop_stack

-- | Set <a> to <b> == <c>
_eq s a b c = set_reg s a $ if b == c then 1 else 0
-- | Set <a> to <b> /= <c>
_gt s a b c = set_reg s a $ if b  > c then 1 else 0

-- | Jump to <a>
_jmp s a   = set_ptr a s
-- | Jump to <b> if <a> /= 0
_jt  s a b = if a /= 0 then set_ptr b s else inc_ptr 3 s
-- | Jump to <b> if <a> == 0
_jf  s a b = if a == 0 then set_ptr b s else inc_ptr 3 s

-- | Arithmetic operations
_add  s a b c = set_reg s a $ mod (b + c) 32768
_mult s a b c = set_reg s a $ mod (b * c) 32768
_mod  s a b c = set_reg s a $ mod b c
_and  s a b c = set_reg s a $ b .&. c
_or   s a b c = set_reg s a $ b .|. c
_not  s a b   = set_reg s a $ mod (clearBit (complement b) 15) 32768

-- | Set <a> to mem <b>
_rmem s a b = set_reg s a $ get_mem s b
-- | Set mem <a> to <b>
_wmem = set_mem

-- | Push the next the next instruction index into the stack and jump to <ptr>
_call s ptr = s {
  _ptr   = ptr,
  _trace = ptr : _trace s,
  _stack = ((+2) . _ptr $ s) : _stack s
  }

-- | Pop stack and jump to ptr
_ret s
  | null stack = throw_err ERR_EMPTY_STACK
  | otherwise  = s {
    _ptr   = head stack,
    _trace = tail (_trace s),
    _stack = tail stack
  }
  where stack = _stack s

-- | Output a character
_out n
  | isAscii c = putStr [c]
  | otherwise = throw_err_arg ERR_NOT_ASCII n
  where c = chr n

-- | Input a character into r
_in s r
  -- | Fill input and set first character
  | null input = do
    line <- getLine
    let line' = if null line then map ord "look\n" else map ord (line ++ "\n")

    -- | Command
    if (not . null $ line) && (head line == ':')
      then do
        s' <- handle_cmd s $ tail line
        return $ dec_ptr 2 s'
    -- | Input
    else
      return $ ( set_reg s r (head line') ) { _input = tail line' }
  -- | Set character
  | otherwise = return $ ( set_reg s r (head input) ) { _input = tail input }
  where input = _input s

-- |
-- | Execution
-- |

exec :: State -> IO ()
exec s
  -- | Pointer out of bounds
  | ptr < 0 || ptr >= V.length (_mem s) = throw_err ERR_PTR_OOB
  -- | Halt
  | _is_halt s = exitSuccess
  -- | I/O opcodes
  | op == 19 = do
    _out a'
    exec . inc_ptr 2 $ s
  | op == 20 = do
    s' <- _in s a
    exec . inc_ptr 2 $ s'
  -- | Other opcodes
  | otherwise  = exec $ case op of
    0  -> inc_ptr 1 $ _halt s
    1  -> inc_ptr 3 $ _set  s a b'
    2  -> inc_ptr 2 $ _push s a'
    3  -> inc_ptr 2 $ _pop  s a
    4  -> inc_ptr 4 $ _eq   s a b' c'
    5  -> inc_ptr 4 $ _gt   s a b' c'
    6  -> _jmp s a'
    7  -> _jt  s a' b'
    8  -> _jf  s a' b'
    9  -> inc_ptr 4 $ _add  s a  b' c'
    10 -> inc_ptr 4 $ _mult s a  b' c'
    11 -> inc_ptr 4 $ _mod  s a  b' c'
    12 -> inc_ptr 4 $ _and  s a  b' c'
    13 -> inc_ptr 4 $ _or   s a  b' c'
    14 -> inc_ptr 3 $ _not  s a  b'
    15 -> inc_ptr 3 $ _rmem s a  b'
    16 -> inc_ptr 3 $ _wmem s a' b'
    17 -> _call s a'
    18 -> _ret  s
    -- | Noop
    21 -> inc_ptr 1 s
    _  -> throw_err_arg ERR_INVALID_OPCODE op
  where ptr = _ptr s
        op  = _mem s V.! ptr
        a   = _mem s V.! (ptr+1)
        b   = _mem s V.! (ptr+2)
        c   = _mem s V.! (ptr+3)
        a'  = get_val s a
        b'  = get_val s b
        c'  = get_val s c


main :: IO ()
main = do
  args <- getArgs
  input <- H.load_bin $ if null args then "../bin/challenge.bin" else head args
  let mem = H.b8_to_b16 input
  let s   = State {
      _mem       = V.fromList mem
    , _reg       = V.fromList [ 0 | _ <- [0..7] ]
    , _stack     = []
    , _ptr       = 0
    , _trace     = []
    , _ptr_trace = []
    , _input     = []
    , _is_debug  = False
    , _is_halt   = False
  }

  exec s

