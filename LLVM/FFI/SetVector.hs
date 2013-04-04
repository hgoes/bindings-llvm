module LLVM.FFI.SetVector
       (SetVector()
       ,SetVectorC(..)
       ,setVectorToList
       ,Const_iterator()
       ,SetVectorIteratorC(..)
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr

class SetVectorC t where
  setVectorEmpty :: Ptr (SetVector t) -> IO Bool
  setVectorBegin :: Ptr (SetVector t) -> IO (Ptr (Const_iterator t))
  setVectorEnd :: Ptr (SetVector t) -> IO (Ptr (Const_iterator t))

class SetVectorIteratorC t where
  setVectorIteratorDeref :: Ptr (Const_iterator t) -> IO t
  setVectorIteratorNext :: Ptr (Const_iterator t) -> IO (Ptr (Const_iterator t))
  setVectorIteratorEq :: Ptr (Const_iterator t) -> Ptr (Const_iterator t) -> IO Bool

setVectorToList :: (SetVectorC t,SetVectorIteratorC t) => Ptr (SetVector t) -> IO [t]
setVectorToList ptr = do
  begin <- setVectorBegin ptr
  end <- setVectorEnd ptr
  fillSet begin end
  where
    fillSet b e = do
      finished <- setVectorIteratorEq b e
      if finished
        then return []
        else (do
                 ref <- setVectorIteratorDeref b
                 nxt <- setVectorIteratorNext b
                 rest <- fillSet nxt e
                 return (ref:rest))
