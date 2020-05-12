module Main where


import qualified Data.ByteString as BS

import Control.Monad ( void )

import System.Environment ( getArgs )

import Fungoid.Interpreter93


main :: IO ()
main = do
  args <- getArgs

  case args of
    [file, seed] -> do
      f <- BS.readFile file
      void $ run f (read seed)
    
    _ -> putStrLn "Usage: Fungoid [file] [seed]"

