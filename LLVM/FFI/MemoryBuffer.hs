module LLVM.FFI.MemoryBuffer 
       (MemoryBuffer(),
        deleteMemoryBuffer,
        getBufferSize,
        getFileMemoryBuffer,
        getFileMemoryBufferSimple
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.OwningPtr
import LLVM.FFI.CPP.UniquePtr
import LLVM.FFI.ErrorCode
import LLVM.FFI.ErrorOr
import LLVM.FFI.StringRef
import Foreign
import Foreign.C

#include "Helper.h"

getBufferSize :: Ptr MemoryBuffer -> IO Integer
getBufferSize buf = fmap fromIntegral $ getBufferSize_ buf

#if HS_LLVM_VERSION<305
SPECIALIZE_OWNINGPTR(MemoryBuffer,capi)
#else
SPECIALIZE_UNIQUEPTR(MemoryBuffer,capi)
SPECIALIZE_ERROROR(MemoryBuffer,Unique_ptr MemoryBuffer,capi)
#endif

getFileMemoryBufferSimple :: String -> IO (Maybe (Ptr MemoryBuffer))
getFileMemoryBufferSimple name = do
  str <- newStringRef name
#if HS_LLVM_VERSION<209
  res' <- getFileMemoryBuffer str
  let res = if res'==nullPtr
            then Nothing
            else Just res'
#elif HS_LLVM_VERSION<305
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
#else
  errc <- getFileMemoryBuffer str (-1) True
  isErr <- errorOrIsError errc
  res <- if isErr
         then return Nothing
         else (do
                  uniq <- errorOrGet errc
                  buf <- releaseUniquePtr uniq
                  return (Just buf))
#endif    
  deleteStringRef str
  return res
