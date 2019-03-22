module Helper where

import Data.Word (Word8)
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed  as V
import System.IO
import DataTypes

-- | Convert to int
to_int :: [Char] -> Int
to_int n = read n :: Int

-- | Load binary file
load_bin :: [Char] -> IO [Word8]
load_bin path = BS.readFile path >>= (return . BS.unpack)

-- | Convert an array of 8-bit integers into 16-bit integers
b8_to_b16 :: [Word8] -> [Int]
b8_to_b16 = map to_b16 . chunksOf 2
    where to_b16 [h, l] = (fromIntegral l :: Int) * 256 + (fromIntegral h :: Int)

-- | Decompile into assembly
decompile_asm :: [Int] -> Int -> [(Int, Opcode)]
decompile_asm [] _ = []
decompile_asm (n:mem) i = (i, op) : decompile_asm (drop len mem) (i+len+1)
  where to_int  n = fromIntegral n ::Int
        to_int' n = Int' $ to_int n
        to_arg n
          | n >= 32768 && n <= 32775 = Reg $ to_int (n-32768)
          | n >= 32776               = Null
          | otherwise                = Val n
        [a, b, c]    = take 3 mem
        [a', b', c'] = map to_arg [a, b, c]
        (op, len) = case n of
                      0  -> (Halt                , 0)
                      1  -> (Set  a' b'          , 2)
                      2  -> (Push a'             , 1)
                      3  -> (Pop  a'             , 1)
                      4  -> (Eq   a' b' c'       , 3)
                      5  -> (Gt   a' b' c'       , 3)
                      6  -> (Jmp  (to_int' a)    , 1)
                      7  -> (Jt   a' (to_int' b) , 2)
                      8  -> (Jf   a' (to_int' b) , 2)
                      9  -> (Add  a' b' c'       , 3)
                      10 -> (Mult a' b' c'       , 3)
                      11 -> (Mod  a' b' c'       , 3)
                      12 -> (And  a' b' c'       , 3)
                      13 -> (Or   a' b' c'       , 3)
                      14 -> (Not  a' b'          , 2)
                      15 -> (Rmem a' b'          , 2)
                      16 -> (Wmem a' b'          , 2)
                      17 -> (Call a'             , 1)
                      18 -> (Ret                 , 0)
                      19 -> (Out  a'             , 1)
                      20 -> (In   a'             , 1)
                      21 -> (Noop                , 0)
                      _  -> (Trash               , 0)

-- | Decompile into ASM and log it into a file
log_asm :: State -> [Char] -> IO ()
log_asm s filename = do
  let ops = decompile_asm (V.toList . _mem $ s) 0
  let lines = map (\(n, op) ->
          (if n == _ptr s then "-> " else "")
          ++ show n
          ++ "  | " 
          ++ show op
        ) ops
  h <- openFile filename WriteMode
  -- | Log VM state
  hPutStrLn h $ "\n" ++ show s ++ "\n"
  -- | Log decompiled code
  mapM_ (hPutStrLn h) lines
  hClose h

save :: State -> [Char] -> IO ()
save s filename = do
  h <- openFile ("../states/" ++ filename ++ ".txt") WriteMode
  hPrint h $ V.toList (_mem s)
  hPrint h $ V.toList (_reg s)
  hPrint h $ _stack s
  hPrint h $ _ptr s
  hPrint h $ _trace s
  hPrint h $ _ptr_trace s
  hPrint h $ _input s
  hPrint h $ _is_debug s
  hPrint h $ _is_halt s
  hClose h

load :: [Char] -> IO State
load filename = do
  h    <- openFile ("../states/" ++ filename ++ ".txt") ReadMode
  file <- hGetContents h
  let file' = lines file
  return $ State {
      _mem       = V.fromList (read (file'!!0) :: [Int])
    , _reg       = V.fromList (read (file'!!1) :: [Int])
    , _stack     = read (file'!!2) :: [Int]
    , _ptr       = read (file'!!3) :: Int
    , _trace     = read (file'!!4) :: [Int]
    , _ptr_trace = read (file'!!5) :: [Int]
    , _input     = read (file'!!6) :: [Int]
    , _is_debug  = read (file'!!7)
    , _is_halt   = read (file'!!8)
  }

-- | Whether the value is a trash value
is_trash :: (Int, Opcode) -> Bool
is_trash (_, op) = op == Trash

-- | Whether the opcode implies a jumping operation
has_jmp :: (Int, Opcode) -> Bool
has_jmp (_, op) = case op of
  Jmp   _ -> True
  Jt  _ _ -> True
  Jf  _ _ -> True
  Call  _ -> True
  _       -> False
  
