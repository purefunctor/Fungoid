module Fungoid.Machine
  ( Direction(..)
  , Machine(..)
  , mkMachine
  )
where


import Data.ByteString ( ByteString )

import Fungoid.Stack
import Fungoid.Torus

import System.Random


-- Represents the cardinal direction where the
-- instruction pointer is currently moving towards.
data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)


-- Encapsulates a Fungoid Machine with the type `a`
-- propagated to the Torus, Coord, and Stack types.
data Machine a = Machine
  { torus     :: Torus a
  , position  :: Coord a
  , direction :: Direction
  , stack     :: Stack a
  , strMode   :: Bool
  , generator :: StdGen
  , randInts  :: [Int]
  }


-- Show instance for debugging purposes.
instance (Integral a, Show a) => Show (Machine a) where
  show (Machine _ p d s m _ _) =
    concat [ "Machine {"
           , "Stack: ", show s, ", "
           , "Position: ", show p, ", "
           , "Direction: ", show d, ", "
           , "String Mode: ", show m, "}"
           ]


-- Primary constructor for building a Machine type.
mkMachine :: (Integral a) => Torus a -> Int -> Machine a
mkMachine tr sd =
  let t = tr
      p = (0, 0)
      d = East
      s = mkStack
      m = False
      g = mkStdGen sd
      r = randoms g
  in Machine t p d s m g r

