module Fungoid.Instructions
  ( Instruction
  , Evaluation
  , runInstruction
  , sPush
  , sPop
  , sDuplicate
  , sSwap
  , sDiscard
  , sAdd
  , sSub
  , sMul
  , sDiv
  , sMod
  , sNot
  , sGT
  , sStore
  , dNorth
  , dSouth
  , dEast
  , dWest
  , dRand
  , dHori
  , dVert
  , ioPutInt
  , ioPutChar
  , ioGetInt
  , ioGetChar
  , tPut
  , tGet
  , pMove
  , mFlip
  )
where


import Data.Char ( chr , ord , isDigit )
import Data.Word ( Word8 )

import Control.Monad ( replicateM_ , void )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Except ( ExceptT , runExceptT , throwError )
import Control.Monad.State ( StateT , runStateT , get , gets , put , modify )

import Fungoid.Machine
import Fungoid.Stack
import Fungoid.Torus


-- Represents the principal monad stack used to perform
-- operations on the Machine type. A short-circuiting
-- stateful computation with access to IO.
type Instruction a = StateT (Machine a) (ExceptT (MachineFail a) IO)


-- Represents the type to be returned when an Instruction
-- fails its execution.
type MachineFail a = (String, Machine a)


-- Represents the innermost type returned after running
-- an Instruction inside the IO monad.
type Evaluation a e = Either (MachineFail a) (e, Machine a)


-- Runs an Instruction on a given Machine in the IO monad.
runInstruction
  :: (Integral a) => Instruction a e -> Machine a -> IO (Evaluation a e)
runInstruction = (runExceptT .) . runStateT


-- LOW-LEVEL STACK INSTRUCTIONS


sPush :: (Integral a) => a -> Instruction a ()
sPush n = modify $ \m -> m {stack = push n $ stack m}


sPop :: (Integral a) => Instruction a a
sPop = do
  m <- get

  let (n, ns) = pop $ stack m

  put $ m {stack = ns}
  return n


-- HIGH-LEVEL STACK INSTRUCTIONS


-- ':'
sDuplicate :: (Integral a) => Instruction a ()
sDuplicate = sPop >>= replicateM_ 2 . sPush


-- '\'
sSwap :: (Integral a) => Instruction a ()
sSwap = do
  x <- sPop
  y <- sPop
  sPush x
  sPush y


-- '$'
sDiscard :: (Integral a) => Instruction a ()
sDiscard = void sPop


-- Perform a unary function on one item popped from the stack
sUnary :: (Integral a) => (a -> a) -> Instruction a ()
sUnary f = sPop >>= sPush . f


-- Perform a binary function on two items popped from the stack
sBinary :: (Integral a) => (a -> a -> a) -> Instruction a ()
sBinary f = f <$> sPop <*> sPop >>= sPush


-- '+'
sAdd :: (Integral a) => Instruction a ()
sAdd = sBinary (+)


-- '-'
sSub :: (Integral a) => Instruction a ()
sSub = sBinary subtract


-- '*'
sMul :: (Integral a) => Instruction a ()
sMul = sBinary (*)


-- '/'
sDiv :: (Integral a) => Instruction a ()
sDiv = sBinary $ \a b -> if a == 0 then 0 else b `div` a


-- '%'
sMod :: (Integral a) => Instruction a ()
sMod = sBinary $ \a b -> if a == 0 then 0 else b `rem` a


-- '!'
sNot :: (Integral a) => Instruction a ()
sNot = sUnary $ \n -> if n == 0 then 1 else 0


-- '`'
sGT :: (Integral a) => Instruction a ()
sGT = sBinary $ \a b -> if b > a then 1 else 0


-- Stores current cell being pointed to into the stack
-- based on the current mode.
sStore :: (Integral a) => Word8 -> Instruction a ()
sStore w = do
  m <- get

  if strMode m
    then sPush $ fromIntegral w
    else if w >= 48 && w <= 57
           then sPush $ fromIntegral $ w - 48
           else throwError ("sStore: Invalid Character.", m)


-- DIRECTION INSTRUCTIONS


-- Helper function for changing directions.
changeDirection :: (Integral a) => Direction -> Machine a -> Machine a
changeDirection d m = m {direction = d}


-- '^'
dNorth :: (Integral a) => Instruction a ()
dNorth = modify $ changeDirection North


-- 'v'
dSouth :: (Integral a) => Instruction a ()
dSouth = modify $ changeDirection South


-- '<'
dEast :: (Integral a) => Instruction a ()
dEast = modify $ changeDirection East


-- '>'
dWest :: (Integral a) => Instruction a ()
dWest = modify $ changeDirection West


-- '?'
dRand :: (Integral a) => Instruction a ()
dRand = do
  m <- get

  let (n : r) = randInts m
  let i = abs n `rem` 4
  let d = [North, South, East, West] !! i

  put $ m {direction = d, randInts = r}


-- '_'
dHori :: (Integral a) => Instruction a ()
dHori = sPop >>= \n -> if n == 0 then dEast else dWest


-- '|'
dVert :: (Integral a) => Instruction a ()
dVert = sPop >>= \n -> if n == 0 then dSouth else dNorth


-- IO INSTRUCTIONS


-- '.'
ioPutInt :: (Integral a, Show a) => Instruction a ()
ioPutInt = sPop >>= liftIO . putStr . (++ " ") . show


-- ','
ioPutChar :: (Integral a) => Instruction a ()
ioPutChar = do
  m <- get
  n <- sPop

  if n >= 0 && n <= 127
    then liftIO . putChar . chr . fromIntegral $ n
    else throwError ("ioPutChar: Invalid ASCII Value.", m)


-- '&'
ioGetInt :: (Integral a, Read a) => Instruction a ()
ioGetInt = do
  m <- get
  s <- liftIO getLine

  if all isDigit s
    then sPush $ read s
    else throwError ("ioGetInt: Expecting Integer Input.", m)


-- '~'
ioGetChar :: (Integral a) => Instruction a ()
ioGetChar = do
  m <- get
  s <- liftIO getLine


  if length s == 1
    then sPush . fromIntegral . ord . head $ s
    else throwError ("ioGetChar: Expecting Single ASCII Character.", m)

-- 'p'
tPut :: (Integral a) => Instruction a ()
tPut = do
  m0 <- get

  y <- sPop
  x <- sPop
  v <- sPop

  m1 <- get

  let t = torus m1

  if inBounds (x, y) t && v >= 0 && v <= 127
    then let t' = torusSet (x, y) (fromIntegral v) t
         in put $ m1 {torus = t'}
    else throwError ("tPut: Out Of Bounds.", m0)


-- 'g'
tGet :: (Integral a) => Instruction a ()
tGet = do
  m <- get

  y <- sPop
  x <- sPop

  t <- gets torus

  case torusGet (x, y) t of
    (Just x) -> sPush $ fromIntegral x
    Nothing  -> throwError ("tGet: Out Of Bounds.", m)


-- MOVEMENT INSTRUCTIONS


-- Used for movement, also for bridge '#'
pMove :: (Integral a) => Instruction a ()
pMove = do
  m <- get

  let (x, y) = position m
  let d      = direction m

  let (xs, xe) = xBound $ torus m
  let (ys, ye) = yBound $ torus m

  let x' | d == East = if x == xe then xs else x + 1
         | d == West = if x == xs then xe else x - 1
         | otherwise = x

  let y' | d == North = if y == ys then ye else y - 1
         | d == South = if y == ye then ys else y + 1
         | otherwise  = y

  put $ m {position = (x', y')}


-- MODE INSTRUCTIONS


-- '"'
mFlip :: (Integral a) => Instruction a ()
mFlip = modify $ \m -> m {strMode = not $ strMode m}

