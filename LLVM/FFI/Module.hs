module LLVM.FFI.Module 
       (Module()
       ,newModule
       ,deleteModule
       ,getGlobalList
       ,getFunctionList
       ,moduleDump
       ,parseIR
       ,writeBitCodeToFile
       ) where

import LLVM.FFI.Interface
import Foreign
import Foreign.C

writeBitCodeToFile :: Ptr Module -> String -> IO Bool
writeBitCodeToFile md name = withCString name
                             $ \str -> do
                               res <- writeBitCodeToFile_ md str
                               return $ res==0

foreign import capi "extra.h writeBitCodeToFile"
  writeBitCodeToFile_ :: Ptr Module -> CString -> IO CInt