module LLVM.FFI.StringRef 
       (StringRef()
       ,newStringRefEmpty
       ,newStringRef
       ,deleteStringRef
       ,stringRefSize
       ,stringRefData) where

import LLVM.FFI.Interface

import Foreign
import Foreign.C

newStringRef :: String -> IO (Ptr StringRef)
newStringRef str = withCString str newStringRef_

stringRefSize :: Ptr StringRef -> IO Integer
stringRefSize ptr = fmap toInteger $ stringRefSize_ ptr

stringRefData :: Ptr StringRef -> IO String
stringRefData ptr = stringRefData_ ptr >>= peekCString