#if HS_LLVM_VERSION >= 209
module LLVM.FFI.ArrayRef 
       (ArrayRef()
       ,ArrayRefC(..)
       ,newArrayRef
       ,arrayRefSize
       ) where

import Foreign
import Foreign.C

import LLVM.FFI.Interface

#include "Helper.h"

class ArrayRefC a where
  newArrayRef' :: Ptr a -> CSize -> IO (Ptr (ArrayRef a))
  newArrayRefEmpty :: IO (Ptr (ArrayRef a))
  arrayRefSize' :: Ptr (ArrayRef a) -> IO CSize
#if HS_LLVM_VERSION >= 300
  arrayRefEquals :: Ptr (ArrayRef a) -> Ptr (ArrayRef a) -> IO Bool
#endif
  arrayRefIndex' :: Ptr (ArrayRef a) -> CSize -> IO (Ptr a)
  deleteArrayRef :: Ptr (ArrayRef a) -> IO ()

newArrayRef :: ArrayRefC a => Ptr a -> Integer -> IO (Ptr (ArrayRef a))
newArrayRef ptr size = newArrayRef' ptr (fromIntegral size)

arrayRefSize :: ArrayRefC a => Ptr (ArrayRef a) -> IO Integer
arrayRefSize ptr = fmap fromIntegral (arrayRefSize' ptr)

SPECIALIZE_ARRAYREF(CChar)
#if HS_LLVM_VERSION>=300
instance ArrayRefC Word64 where
  newArrayRef' = newArrayRefWord64
  newArrayRefEmpty = newArrayRefEmptyWord64
  arrayRefSize' = arrayRefSizeWord64
  arrayRefEquals = arrayRefEqualsWord64
  arrayRefIndex' = arrayRefIndexWord64
  deleteArrayRef = deleteArrayRefWord64
#else
instance ArrayRefC Word64 where
  newArrayRef' = newArrayRefWord64
  newArrayRefEmpty = newArrayRefEmptyWord64
  arrayRefSize' = arrayRefSizeWord64
  arrayRefIndex' = arrayRefIndexWord64
  deleteArrayRef = deleteArrayRefWord64
#endif

#else
module LLVM.FFI.ArrayRef () where
#endif
