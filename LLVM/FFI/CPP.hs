module LLVM.FFI.CPP 
       (Vector()
       ,VectorC(..)
       ,Const_iterator()
       ,vectorToList
       ,withVector
       ,vectorIteratorToList
       ,Pair(..)
       ,PairC(..)
       ,pairToTuple
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Array

class Storable c => VectorC c where
  vectorBegin :: Ptr (Vector c) -> IO (Ptr (Const_iterator c))
  vectorEnd :: Ptr (Vector c) -> IO (Ptr (Const_iterator c))
  vectorIteratorDeref :: Ptr (Const_iterator c) -> IO c
  vectorIteratorNext :: Ptr (Const_iterator c) -> IO (Ptr (Const_iterator c))
  vectorIteratorEq :: Ptr (Const_iterator c) -> Ptr (Const_iterator c) -> IO Bool
  newVector :: Ptr c -> Ptr c -> IO (Ptr (Vector c))
  vectorClear :: Ptr (Vector c) -> IO ()
  vectorPushBack :: Ptr (Vector c) -> Ptr c -> IO ()
  vectorResize :: Ptr (Vector c) -> CUInt -> IO ()
  vectorIndex :: Ptr (Vector c) -> CSize -> IO (Ptr c)
  vectorSize :: Ptr (Vector c) -> IO CSize
  vectorDelete :: Ptr (Vector c) -> IO ()

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

withVector :: VectorC c => [c] -> (Ptr (Vector c) -> IO a) -> IO a
withVector lst f = withArrayLen lst $
                   \len arr -> do
                     let end = advancePtr arr len
                     vec <- newVector arr end
                     res <- f vec
                     vectorDelete vec
                     return res

class PairC a b where
  pairSize :: Pair a b -> CSize
  pairFirst :: Ptr (Pair a b) -> IO a
  pairSecond :: Ptr (Pair a b) -> IO b
  pairSetFirst :: Ptr (Pair a b) -> a -> IO ()
  pairSetSecond :: Ptr (Pair a b) -> b -> IO ()

pairToTuple :: PairC a b => Ptr (Pair a b) -> IO (a,b)
pairToTuple pair = do
  x <- pairFirst pair
  y <- pairSecond pair
  return (x,y)
