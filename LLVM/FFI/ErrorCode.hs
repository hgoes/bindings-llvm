module LLVM.FFI.ErrorCode 
       (Error_code()
       ,deleteErrorCode
       ,errorCodeValue
       ) where

import Foreign
import Foreign.C
import LLVM.FFI.Interface

errorCodeValue :: Ptr Error_code -> IO Integer
errorCodeValue ptr = fmap toInteger (errorCodeValue_ ptr)