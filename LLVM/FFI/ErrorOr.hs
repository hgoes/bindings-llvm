#if HS_LLVM_VERSION>=305
module LLVM.FFI.ErrorOr (ErrorOr(),ErrorOrC(..)) where

import Foreign
import LLVM.FFI.Interface

class ErrorOrC a where
  errorOrIsSuccess :: Ptr (ErrorOr a) -> IO Bool
  errorOrGetError :: Ptr (ErrorOr a) -> IO (Ptr Error_code)
  errorOrGet :: Ptr (ErrorOr a) -> IO (Ptr a)

#else
module LLVM.FFI.ErrorOr () where
#endif
