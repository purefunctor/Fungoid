module Fungoid.Torus
  ( Coord
  , Space
  , Torus
  , mkTorus
  , asTorus
  , torusGet
  , torusSet
  , inBounds
  )
where


import Data.Word ( Word8 )

import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Map ( Map )
import qualified Data.Map as Map


-- Represents coordinates used in the Torus space.
type Coord a = (a, a)


-- Represents the Torus space as a mapping.
type Space a = Map (Coord a) Word8


-- Encapsulates the Torus' space and boundaries.
data Torus a = Torus
  { space  :: Space a
  , xBound :: Coord a
  , yBound :: Coord a
  } deriving (Show)


-- Primary constructor for building a Torus type.
mkTorus :: (Integral a) => Coord a -> Coord a -> Torus a
mkTorus = Torus (Map.fromList [])


-- Secondary constructor for building a Torus type
-- from the contents of a ByteString.
asTorus :: (Integral a) => ByteString -> Torus a -> Torus a
asTorus b t = t {space = s}
 where
  w = B.unpack <$> C.lines b
  s = Map.fromList [ ((x, y), v)
                   | (x, r) <- zip [0 ..] w
                   , (y, v) <- zip [0 ..] r
                   ]


-- `get` operation for the Torus type.
torusGet :: (Integral a) => Coord a -> Torus a -> Maybe Word8
torusGet c t
  | inBounds c t = Just i
  | otherwise    = Nothing
  where
  i = case Map.lookup c $ space t of
        (Just x) -> x
        Nothing  -> 32


-- `set` operation for the Torus type.
torusSet :: (Integral a) => Coord a -> Word8 -> Torus a -> Torus a
torusSet c w t = t {space = Map.insert c w (space t)}


-- Checks whether a coordinate pair is within the bounds
-- of a Torus.
inBounds :: (Integral a) => Coord a -> Torus a -> Bool
inBounds (x, y) (Torus s (xs, xe) (ys, ye)) = vx && vy
 where
  vx = x >= xs && x <= xe
  vy = y >= ys && y <= ye

