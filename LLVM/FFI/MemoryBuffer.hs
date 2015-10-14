module LLVM.FFI.MemoryBuffer 
       (MemoryBuffer(),
        deleteMemoryBuffer,
        getBufferSize,
        getFileMemoryBuffer,
        getFileMemoryBufferSimple,
        getStdInMemoryBuffer,
        getStdInMemoryBufferSimple
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

getFileMemoryBufferSimple :: String -> IO (Either String (Ptr MemoryBuffer))
getFileMemoryBufferSimple name = do
  str <- newStringRef name
#if HS_LLVM_VERSION<209
  res' <- getFileMemoryBuffer str
  let res = if res'==nullPtr
            then Left "Unknown error"
            else Right res'
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
                  return $ Right buf)
         else (do
                  msg <- errorCodeMessage errc
                  return $ Left msg)
  deleteErrorCode errc
  deleteOwningPtr ptr
#else
  errc <- getFileMemoryBuffer str (-1) True
  isSucc <- errorOrIsSuccess errc
  res <- if not isSucc
         then (do
                  msg <- errorOrGetError errc >>= errorCodeMessage
                  return $ Left msg)
         else (do
                  uniq <- errorOrGet errc
                  buf <- releaseUniquePtr uniq
                  return $ Right buf)
#endif    
  deleteStringRef str
  return res

getStdInMemoryBufferSimple :: IO (Either String (Ptr MemoryBuffer))
getStdInMemoryBufferSimple = do
#if HS_LLVM_VERSION<=208
  res' <- getStdInMemoryBuffer
  return $ if res'==nullPtr
           then Left "Unknown error"
           else Right res'
#elif HS_LLVM_VERSION<=304
  ptr <- newOwningPtr nullPtr
  errc <- getStdInMemoryBuffer ptr
  errv <- errorCodeValue errc
  if errv==0
  then do
    res <- takeOwningPtr ptr
    deleteOwningPtr ptr
    return (Right res)
  else do
    msg <- errorCodeMessage errc
    deleteOwningPtr ptr
    return (Left msg)
#else
  errc <- getStdInMemoryBuffer
  isSucc <- errorOrIsSuccess errc
  if not isSucc
    then (do
            msg <- errorOrGetError errc >>= errorCodeMessage
            return $ Left msg)
    else (do
            uniq <- errorOrGet errc
            buf <- releaseUniquePtr uniq
            return $ Right buf)
#endif
