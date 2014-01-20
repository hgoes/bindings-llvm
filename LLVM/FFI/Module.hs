module LLVM.FFI.Module 
       (Module()
        -- * Public Member Functions
        -- ** Constructors
       ,newModule
       ,deleteModule
        -- ** Module Level Accessors
       ,moduleGetContext
        -- ** Utility functions for printing and dumping Module objects
       ,moduleDump
        -- * Generic Value Accessors
       ,moduleGetNamedValue
       ,moduleGetTypeByName
        -- * Globals list, functions list, and symbol table
       ,moduleGetGlobalList
       ,moduleGetFunctionList
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
