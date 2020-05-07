module Fungoid.Stack
  ( Stack
  , mkStack
  , push
  , pop
  )
where


-- Represents a general Stack type.
type Stack a = [a]


-- An empty Stack for all Integral types.
mkStack :: (Integral a) => Stack a
mkStack = []


-- Pushes an item to the stack.
push :: (Integral a) => a -> Stack a -> Stack a
push n ns = (n : ns)


-- Pops an item from the stack.
pop :: (Integral a) => Stack a -> (a, Stack a)
pop []       = (0, [])
pop (n : ns) = (n, ns)

