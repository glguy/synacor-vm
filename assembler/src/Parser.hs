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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad
import Text.Parsec.Prim
import qualified Tokens as T
import Lexer
import Data.Functor.Identity

type Parser = ParsecT [T.Located T.Token] () Identity

mktoken :: (T.Token -> Maybe a) -> Parser a
mktoken f = tokenPrim (show . T.locThing) (\p t _ -> asSourcePos (T.locPosition t) p) (f . T.locThing)

instr :: Parser Instruction
instr = choice
  [ Halt <$ match T.Halt
  , Set  <$ match T.Set  <*> reg <*> val
  , Push <$ match T.Push <*> val
  , Pop  <$ match T.Pop  <*> reg
  , Eq   <$ match T.Eq   <*> reg <*> val <*> val
  , Gt   <$ match T.Gt   <*> reg <*> val <*> val
  , Jmp  <$ match T.Jmp  <*> val
  , Jt   <$ match T.Jt   <*> val <*> val
  , Jf   <$ match T.Jf   <*> val <*> val
  , Add  <$ match T.Add  <*> reg <*> val <*> val
  , Mult <$ match T.Mult <*> reg <*> val <*> val
  , Mod  <$ match T.Mod  <*> reg <*> val <*> val
  , And  <$ match T.And  <*> reg <*> val <*> val
  , Or   <$ match T.Or   <*> reg <*> val <*> val
  , Not  <$ match T.Not  <*> reg <*> val
  , Rmem <$ match T.Rmem <*> reg <*> val
  , Wmem <$ match T.Wmem <*> val <*> val
  , Call <$ match T.Call <*> val
  , Ret  <$ match T.Ret
  , Out  <$ match T.Out  <*> val
  , In   <$ match T.In   <*> reg
  , Noop <$ match T.Noop
  ]

val :: Parser Val
val = choice [ Number <$> number
             , Label  <$> labelVal
             , Reg    <$> reg
             ]

reg :: Parser Reg
reg = mktoken $ \case
  T.A -> Just A
  T.B -> Just B
  T.C -> Just C
  T.D -> Just D
  T.E -> Just E
  T.F -> Just F
  T.G -> Just G
  T.H -> Just H
  _   -> Nothing

labelVal :: Parser Text
labelVal = mktoken $ \case
  T.Label l -> Just l
  _         -> Nothing

number :: Parser Int
number = mktoken $ \case
  T.Number n -> Just n
  _          -> Nothing

stringlit :: Parser Text
stringlit = mktoken $ \case
  T.String s -> Just s
  _          -> Nothing

match :: T.Token -> Parser ()
match tok = mktoken $ \t -> guard (tok==t)

endOfFile :: Parser ()
endOfFile = match T.EOF

colon :: Parser ()
colon = match T.Colon

asmpart :: Parser AsmPart
asmpart = choice
  [ Instruction <$> instr
  , FileLabel   <$> labelVal <* colon
  , Literal     <$> number
  , String      <$> stringlit
  ]

asmfile :: Parser [AsmPart]
asmfile = many asmpart <* endOfFile

asSourcePos :: T.Position -> SourcePos -> SourcePos
asSourcePos tpos pos = setSourceColumn (setSourceLine pos (T.posLine tpos)) (T.posColumn tpos)

parseFile :: FilePath -> IO [AsmPart]
parseFile fp =
  do txt <- Text.readFile fp
     case parse asmfile fp (scanTokens txt) of
       Left e -> fail (show e)
       Right xs -> return xs
