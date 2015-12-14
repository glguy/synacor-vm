module Main where

import Assembly
import Parser

import qualified Data.ByteString.Lazy as L
import System.Environment

getFileNames :: IO (FilePath, FilePath)
getFileNames =
  do args <- getArgs
     case args of
       [infile,outfile] -> return (infile, outfile)
       _ -> fail "Usage: assembler INFILE OUTFILE"

main :: IO ()
main =
  do (infile, outfile) <- getFileNames
     xs <- parseFile infile
     L.writeFile outfile (assemble xs)
