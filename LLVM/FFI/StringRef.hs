module LLVM.FFI.StringRef 
       (StringRef(..)
       ,newStringRefEmpty
       ,newStringRef
       ,deleteStringRef
       ,stringRefSize
       ,stringRefData
       ,withStringRef) where

import LLVM.FFI.Interface

import Foreign
import Foreign.C

newStringRef :: String -> IO (Ptr StringRef)
newStringRef str = withCString str newStringRef_

stringRefSize :: Ptr StringRef -> IO Integer
stringRefSize ptr = fmap toInteger $ stringRefSize_ ptr

stringRefData :: Ptr StringRef -> IO String
stringRefData ptr = stringRefData_ ptr >>= peekCString

withStringRef :: String -> (Ptr StringRef -> IO a) -> IO a
withStringRef str act = do
  ref <- newStringRef str
  res <- act ref
  deleteStringRef ref
  return res

foreign import ccall unsafe "sizeof_StringRef"
  sizeof_StringRef :: CSize

foreign import ccall unsafe "alignof_StringRef"
  alignof_StringRef :: CSize

foreign import capi "extra.h move_StringRef"
  stringRefMove :: Ptr StringRef -> CString -> IO ()

instance Storable StringRef where
  sizeOf _ = fromIntegral sizeof_StringRef
  alignment _ = fromIntegral alignof_StringRef
  peek ptr = do
    str <- stringRefData ptr
    return $ StringRef str
  poke ptr (StringRef str)
    = withCString str $
      \cstr -> stringRefMove ptr cstr
