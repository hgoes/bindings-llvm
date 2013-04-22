module LLVM.FFI.MemoryBuffer 
       (MemoryBuffer(),
        deleteMemoryBuffer,
        getBufferSize,
        getFileMemoryBuffer,
        getFileMemoryBufferSimple
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.OwningPtr
import LLVM.FFI.ErrorCode
import LLVM.FFI.StringRef
import Foreign
import Foreign.C

#include "Helper.h"

getBufferSize :: Ptr MemoryBuffer -> IO Integer
getBufferSize buf = fmap fromIntegral $ getBufferSize_ buf

SPECIALIZE_OWNINGPTR(MemoryBuffer,capi)

getFileMemoryBufferSimple :: String -> IO (Maybe (Ptr MemoryBuffer))
getFileMemoryBufferSimple name = do
  str <- newStringRef name
  ptr <- newOwningPtr nullPtr
#if HS_LLVM_VERSION>=300
  errc <- getFileMemoryBuffer str ptr (-1) True
#else
  errc <- getFileMemoryBuffer str ptr (-1)
#endif
  err_val <- errorCodeValue errc
  res <- if err_val == 0
         then (do
                  buf <- takeOwningPtr ptr
                  return $ Just buf)
         else return Nothing
  deleteErrorCode errc
  deleteOwningPtr ptr
  deleteStringRef str
  return res