#if HS_LLVM_VERSION>=209
module LLVM.FFI.ErrorCode
       (Error_code()
       ,deleteErrorCode
       ,errorCodeValue
       ,errorCodeMessage
       ) where

import Foreign
import Foreign.C
import LLVM.FFI.Interface
import LLVM.FFI.CPP.String

errorCodeMessage :: Ptr Error_code -> IO String
errorCodeMessage code = do
  cpp_str <- errorCodeMessage_ code
  cstr <- cppStringToString cpp_str
  res <- peekCString cstr
  cppStringDelete cpp_str
  return res

#else
module LLVM.FFI.ErrorCode () where
#endif
