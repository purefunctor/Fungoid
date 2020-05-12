module Fungoid.Interpreter93 where

import Data.ByteString ( ByteString )
import Data.Char ( chr )
import Data.Int ( Int32 )
import Data.Word ( Word8 )

import Data.Maybe ( fromJust )

import Control.Monad ( unless )
import Control.Monad.State ( get )

import Fungoid.Instructions
import Fungoid.Machine
import Fungoid.Torus
import Fungoid.Stack


-- Standard Instruction type using Int32
type Instruction93 = Instruction Int32


-- Standard Evaluation type using Int32
type Evaluation93 e = Evaluation Int32 e


-- Standard Machine type using Int32
type Machine93 = Machine Int32


-- Creates a machine from a given ByteString and
-- an Integer seed.
buildMachine :: ByteString -> Int -> Machine93
buildMachine b n = m
  where
    t = asTorus (mkTorus (0, 79) (0, 24)) b
    m = mkMachine t n


-- Interprets a Word8 while in String mode.
strModeInstruction :: Word8 -> Instruction93 ()
strModeInstruction w = if c == '"' then mFlip else sStore w
  where c = chr $ fromEnum w


-- Interprets a given Word8 in Normal mode.
normalInstruction :: Word8 -> Instruction93 ()
normalInstruction w =
  case (chr $ fromEnum w) of
    '+'  -> sAdd
    '*'  -> sMul
    '-'  -> sSub
    '/'  -> sDiv
    '%'  -> sMod
    '!'  -> sNot
    '`'  -> sGT
    '>'  -> dEast
    '<'  -> dWest
    '^'  -> dNorth
    'v'  -> dSouth
    '?'  -> dRand
    '_'  -> dHori
    '|'  -> dVert
    '"'  -> mFlip
    ':'  -> sDuplicate
    '\\' -> sSwap
    '$'  -> sDiscard
    '.'  -> ioPutInt
    ','  -> ioPutChar
    '&'  -> ioGetInt
    '~'  -> ioGetChar
    '#'  -> pMove
    'p'  -> tPut
    'g'  -> tGet
    '@'  -> return ()
    ' '  -> return ()
    _    -> sStore w


-- Performs the instruction on the current position.
perform :: Bool -> Instruction93 ()
perform f = do
  m <- get

  let t = torus m
  let p = position m
  let w = fromJust $ torusGet p t
  let s = strMode m
  let c = chr $ fromEnum w

  case s of
    True  -> strModeInstruction w
    False -> normalInstruction w

  case f of
    True  -> unless (c == '@') (pMove >> perform f)
    False -> unless (c == '@') pMove


-- Interprets the entire program.
interpret :: Instruction93 ()
interpret = perform True


-- Runs a program from a ByteString and a seed.
run :: ByteString -> Int -> IO (Evaluation93 ())
run b n = runInstruction interpret (buildMachine b n)
