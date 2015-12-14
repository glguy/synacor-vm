{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Assembly where

import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Text.Parsec
import Data.Map (Map)
import Data.Char (ord)
import Data.Foldable
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad
import Text.Parsec.Prim
import qualified Tokens as T
import Lexer
import Data.Functor.Identity

data AsmPart = Instruction Instruction
             | FileLabel Text
             | Literal Int
             | String Text
  deriving (Show, Read)

data Reg = A | B | C | D | E | F | G | H
  deriving (Show, Read, Enum, Bounded)

data Val = Number Int | Reg Reg | Label Text
  deriving (Show, Read)

data Instruction
  = Halt
  | Set Reg Val
  | Push Val
  | Pop Reg
  | Eq Reg Val Val
  | Gt Reg Val Val
  | Jmp Val
  | Jt Val Val
  | Jf Val Val
  | Add Reg Val Val
  | Mult Reg Val Val
  | Mod Reg Val Val
  | And Reg Val Val
  | Or Reg Val Val
  | Not Reg Val
  | Rmem Reg Val
  | Wmem Val Val
  | Call Val
  | Ret
  | Out Val
  | In Reg
  | Noop
  deriving (Show, Read)

class Size a where
  size :: a -> Int

instance Size Instruction where
  size = \case
    Halt {} -> 1
    Set  {} -> 3
    Push {} -> 2
    Pop  {} -> 2
    Eq   {} -> 4
    Gt   {} -> 4
    Jmp  {} -> 2
    Jt   {} -> 3
    Jf   {} -> 3
    Add  {} -> 4
    Mult {} -> 4
    Mod  {} -> 4
    And  {} -> 4
    Or   {} -> 4
    Not  {} -> 3
    Rmem {} -> 3
    Wmem {} -> 3
    Call {} -> 2
    Ret  {} -> 1
    Out  {} -> 2
    In   {} -> 2
    Noop {} -> 1

instance Size AsmPart where
  size = \case
    Instruction i -> size i
    FileLabel  {} -> 0
    Literal    {} -> 1
    String txt    -> Text.length txt + 1

resolveLabels :: [AsmPart] -> Map Text Int
resolveLabels = snd . foldl' aux (0, Map.empty)
  where
  aux (!pos, !acc) x = (pos + size x, acc')
    where
    acc' = case x of
             FileLabel txt
               | Map.member txt acc -> error ("Duplicate label: " ++ show txt)
               | otherwise -> Map.insert txt pos acc
             _ -> acc


buildInstruction :: Map Text Int -> Instruction -> Builder
buildInstruction labs = \case
  Halt       -> word16LE  0
  Set a b    -> word16LE  1 <> buildReg a <> buildVal labs b
  Push a     -> word16LE  2 <> buildVal labs a
  Pop a      -> word16LE  3 <> buildReg a
  Eq a b c   -> word16LE  4 <> buildReg a <> buildVal labs b <> buildVal labs c
  Gt a b c   -> word16LE  5 <> buildReg a <> buildVal labs b <> buildVal labs c
  Jmp a      -> word16LE  6 <> buildVal labs a
  Jt a b     -> word16LE  7 <> buildVal labs a <> buildVal labs b
  Jf a b     -> word16LE  8 <> buildVal labs a <> buildVal labs b
  Add a b c  -> word16LE  9 <> buildReg a <> buildVal labs b <> buildVal labs c
  Mult a b c -> word16LE 10 <> buildReg a <> buildVal labs b <> buildVal labs c
  Mod a b c  -> word16LE 11 <> buildReg a <> buildVal labs b <> buildVal labs c
  And a b c  -> word16LE 12 <> buildReg a <> buildVal labs b <> buildVal labs c
  Or a b c   -> word16LE 13 <> buildReg a <> buildVal labs b <> buildVal labs c
  Not a b    -> word16LE 14 <> buildReg a <> buildVal labs b
  Rmem a b   -> word16LE 15 <> buildReg a <> buildVal labs b
  Wmem a b   -> word16LE 16 <> buildVal labs a <> buildVal labs b
  Call a     -> word16LE 17 <> buildVal labs a
  Ret        -> word16LE 18
  Out a      -> word16LE 19 <> buildVal labs a
  In a       -> word16LE 20 <> buildReg a
  Noop       -> word16LE 21

buildReg :: Reg -> Builder
buildReg r = word16LE (0x8000 + fromIntegral (fromEnum r))

buildVal :: Map Text Int -> Val -> Builder
buildVal labs = \case
  Number n -> word16LE (fromIntegral n)
  Reg    r -> buildReg r
  Label  t -> case Map.lookup t labs of
                Just n -> word16LE (fromIntegral n)
                Nothing -> error ("Bad label: " ++ show t)

buildPart :: Map Text Int -> AsmPart -> Builder
buildPart labs = \case
  Instruction i -> buildInstruction labs i
  FileLabel {}  -> mempty
  Literal n     -> word16LE (fromIntegral n)
  String txt    -> foldMap (word16LE . fromIntegral . ord) (Text.unpack txt)

assemble :: [AsmPart] -> ByteString
assemble xs = toLazyByteString (foldMap (buildPart labs) xs)
  where
  labs = resolveLabels xs
