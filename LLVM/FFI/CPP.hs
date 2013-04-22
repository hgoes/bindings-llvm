module LLVM.FFI.CPP 
       (Vector()
       ,VectorC(..)
       ,Const_iterator()
       ,vectorToList
       ,vectorIteratorToList
       ,Pair()
       ,PairC(..)
       ,pairToTuple
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr
import Foreign.C

class VectorC c where
  vectorBegin :: Ptr (Vector c) -> IO (Ptr (Const_iterator c))
  vectorEnd :: Ptr (Vector c) -> IO (Ptr (Const_iterator c))
  vectorIteratorDeref :: Ptr (Const_iterator c) -> IO c
  vectorIteratorNext :: Ptr (Const_iterator c) -> IO (Ptr (Const_iterator c))
  vectorIteratorEq :: Ptr (Const_iterator c) -> Ptr (Const_iterator c) -> IO Bool

vectorToList :: VectorC c => Ptr (Vector c) -> IO [c]
vectorToList vec = do
  begin <- vectorBegin vec
  end <- vectorEnd vec
  vectorIteratorToList begin end

vectorIteratorToList :: VectorC c => Ptr (Const_iterator c) -> Ptr (Const_iterator c) -> IO [c]
vectorIteratorToList cur end = do
  isEq <- vectorIteratorEq cur end
  if isEq
    then return []
    else (do
             v <- vectorIteratorDeref cur
             nxt <- vectorIteratorNext cur
             vs <- vectorIteratorToList nxt end
             return (v:vs))

class PairC a b where
  pairSize :: Pair a b -> CSize
  pairFirst :: Ptr (Pair a b) -> IO a
  pairSecond :: Ptr (Pair a b) -> IO b

pairToTuple :: PairC a b => Ptr (Pair a b) -> IO (a,b)
pairToTuple pair = do
  x <- pairFirst pair
  y <- pairSecond pair
  return (x,y)