module LLVM.FFI.SmallVector 
       (SmallVector()
       ,SmallVectorC(..)
       ,smallVectorToList
       ) where

import LLVM.FFI.Interface

import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Array

class SmallVectorC t where
  newSmallVector :: IO (Ptr (SmallVector t))
  smallVectorSize :: Ptr (SmallVector t) -> IO CSize
  smallVectorData :: Ptr (SmallVector t) -> IO (Ptr t)

smallVectorToList :: (SmallVectorC t,Storable t) => Ptr (SmallVector t) -> IO [t]
smallVectorToList vec = do
  sz <- smallVectorSize vec
  if sz==0
    then return []
    else (do
             dat <- smallVectorData vec
             peekArray (fromIntegral sz) dat)