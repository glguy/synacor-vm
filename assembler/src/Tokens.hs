-- | This module provides the token type used in the lexer and
-- parser and provides the extra pass to insert layout tokens.
module Tokens
  ( Token(..)
  , Located(..)
  , Position(..)
  , Error(..)
  ) where

import Data.Text (Text)

-- | A position in a text file
data Position = Position
  { posIndex, posLine, posColumn :: {-# UNPACK #-} !Int }
  deriving (Read, Show)

-- | A value annotated with its text file position
data Located a = Located
  { locPosition :: {-# UNPACK #-} !Position
  , locThing    :: !a
  }
  deriving (Read, Show)

instance Functor Located where
  fmap f (Located p x) = Located p (f x)

-- | The token type used by "Config.Lexer" and "Config.Parser"
data Token
  = Label Text
  | Colon
  | String Text
  | Number Int

  | A | B | C | D | E | F | G | H

  | Halt
  | Set
  | Push
  | Pop
  | Eq
  | Gt
  | Jmp
  | Jt
  | Jf
  | Add
  | Mult
  | Mod
  | And
  | Or
  | Not
  | Rmem
  | Wmem
  | Call
  | Ret
  | Out
  | In
  | Noop

  | Error Error

  | EOF
  deriving (Show,Eq)

-- | Types of lexical errors
data Error
  = UntermString
  | UntermFile
  | BadEscape Text
  | NoMatch Char
  deriving (Show,Eq)
