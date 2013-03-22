module LLVM.FFI.ArrayRef 
       (ArrayRef()
       ,ArrayRefC(..)
       ,newArrayRef
       ,arrayRefSize
       ) where

import Foreign
import Foreign.C

import LLVM.FFI.Interface

class ArrayRefC a where
  newArrayRef' :: Ptr a -> CSize -> IO (Ptr (ArrayRef a))
  arrayRefSize' :: Ptr (ArrayRef a) -> IO CSize
  arrayRefEquals :: Ptr (ArrayRef a) -> Ptr (ArrayRef a) -> IO Bool
  arrayRefIndex' :: Ptr (ArrayRef a) -> CSize -> IO (Ptr a)

newArrayRef :: ArrayRefC a => Ptr a -> Integer -> IO (Ptr (ArrayRef a))
newArrayRef ptr size = newArrayRef' ptr (fromIntegral size)

arrayRefSize :: ArrayRefC a => Ptr (ArrayRef a) -> IO Integer
arrayRefSize ptr = fmap fromIntegral (arrayRefSize' ptr)