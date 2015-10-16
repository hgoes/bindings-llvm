module LLVM.FFI.ValueMap
       (ValueMap()
       ,ValueMapC(..)
       ,WeakVH()
       ,newWeakVH
       ,newWeakVHEmpty) where

import LLVM.FFI.Interface
import LLVM.FFI.Value

import Foreign.Ptr

class ValueMapC key value where
  newValueMap :: IO (Ptr (ValueMap key value))
  deleteValueMap :: Ptr (ValueMap key value) -> IO ()
  valueMapEmpty :: Ptr (ValueMap key value) -> IO Bool

newWeakVH :: ValueC v => Ptr v -> IO (Ptr WeakVH)
newWeakVH = newWeakVH_

instance ValueMapC (Ptr Value) WeakVH where
  newValueMap = newValueMapValueToValue
  deleteValueMap = deleteValueMapValueToValue
  valueMapEmpty = valueMapEmptyValueToValue
