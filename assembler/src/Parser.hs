{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Parser (asmfile, parseFile) where

import Assembly

import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Text.Parsec
import Data.Map (Map)
import Data.Char (ord)
import Data.Foldable
import qualified Data.Map as Map
import Control.Monad
import Data.Functor.Identity
import Text.Parsec.Language
import Text.Parsec.Token

type Parser = ParsecT String () Identity

myLanguageDef :: LanguageDef st
myLanguageDef = emptyDef
  { commentLine   = "#"
  , reservedNames = ["HALT", "SET" , "PUSH", "POP", "EQ" , "GT"  , "JMP", "JT"  ,
                     "JF"  , "ADD" , "MULT", "MOD", "AND", "OR"  , "NOT", "RMEM",
                     "WMEM", "CALL", "RET" , "OUT", "IN" , "NOOP",
                     "A", "B", "C", "D", "E", "F", "G", "H"]
  }

tp :: TokenParser st
tp = makeTokenParser myLanguageDef

instr :: Parser Instruction
instr = choice
  [ Halt <$ reserved tp "HALT"
  , Set  <$ reserved tp "SET"  <*> reg <*> val
  , Push <$ reserved tp "PUSH" <*> val
  , Pop  <$ reserved tp "POP"  <*> reg
  , Eq   <$ reserved tp "EQ"   <*> reg <*> val <*> val
  , Gt   <$ reserved tp "GT"   <*> reg <*> val <*> val
  , Jmp  <$ reserved tp "JMP"  <*> val
  , Jt   <$ reserved tp "JT"   <*> val <*> val
  , Jf   <$ reserved tp "JF"   <*> val <*> val
  , Add  <$ reserved tp "ADD"  <*> reg <*> val <*> val
  , Mult <$ reserved tp "MULT" <*> reg <*> val <*> val
  , Mod  <$ reserved tp "MOD"  <*> reg <*> val <*> val
  , And  <$ reserved tp "AND"  <*> reg <*> val <*> val
  , Or   <$ reserved tp "OR"   <*> reg <*> val <*> val
  , Not  <$ reserved tp "NOT"  <*> reg <*> val
  , Rmem <$ reserved tp "RMEM" <*> reg <*> val
  , Wmem <$ reserved tp "WMEM" <*> val <*> val
  , Call <$ reserved tp "CALL" <*> val
  , Ret  <$ reserved tp "RET"
  , Out  <$ reserved tp "OUT"  <*> val
  , In   <$ reserved tp "IN"   <*> reg
  , Noop <$ reserved tp "NOOP"
  ]

val :: Parser Val
val = choice [ Number <$> integer tp
             , Char   <$> charLiteral tp
             , Label  <$> identifier tp
             , Reg    <$> reg
             ]

reg :: Parser Reg
reg = choice
  [ A <$ reserved tp "A"
  , B <$ reserved tp "B"
  , C <$ reserved tp "C"
  , D <$ reserved tp "D"
  , E <$ reserved tp "E"
  , F <$ reserved tp "F"
  , G <$ reserved tp "G"
  , H <$ reserved tp "H"
  ]

asmpart :: Parser AsmPart
asmpart = choice
  [ Instruction <$> instr
  , FileLabel   <$> identifier tp <* colon tp
  , Literal     <$> val
  , String      <$> stringLiteral tp
  ]

asmfile :: Parser [AsmPart]
asmfile = whiteSpace tp *> many asmpart <* eof

parseFile :: FilePath -> IO [AsmPart]
parseFile fp =
  do txt <- readFile fp
     case parse asmfile fp txt of
       Left e -> fail (show e)
       Right xs -> return xs
