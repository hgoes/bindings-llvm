#if HS_LLVM_VERSION>=305
module LLVM.FFI.CPP.UniquePtr (Unique_ptr(),UniquePtrC(..)) where

import Foreign
import LLVM.FFI.Interface

class UniquePtrC a where
  newUniquePtr :: Ptr a -> IO (Ptr (Unique_ptr a))
  deleteUniquePtr :: Ptr (Unique_ptr a) -> IO ()
  getUniquePtr :: Ptr (Unique_ptr a) -> IO (Ptr a)
  releaseUniquePtr :: Ptr (Unique_ptr a) -> IO (Ptr a)
#else
module LLVM.FFI.CPP.UniquePtr () where
#endif
