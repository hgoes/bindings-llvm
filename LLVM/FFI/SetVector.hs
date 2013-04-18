module LLVM.FFI.SetVector
       (SetVector()
       ,SetVectorC(..)
       ,setVectorToList
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.CPP
import Foreign.Ptr

class VectorC t => SetVectorC t where
  setVectorEmpty :: Ptr (SetVector t) -> IO Bool
  setVectorBegin :: Ptr (SetVector t) -> IO (Ptr (Const_iterator t))
  setVectorEnd :: Ptr (SetVector t) -> IO (Ptr (Const_iterator t))

setVectorToList :: (SetVectorC t) => Ptr (SetVector t) -> IO [t]
setVectorToList ptr = do
  begin <- setVectorBegin ptr
  end <- setVectorEnd ptr
  vectorIteratorToList begin end