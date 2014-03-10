module LLVM.FFI.Module 
       (Module()
        -- * Public Member Functions
        -- ** Constructors
       ,newModule
       ,deleteModule
        -- ** Module Level Accessors
       ,moduleGetContext
        -- ** Function Accessors
       ,moduleGetFunction
       ,moduleGetFunctionString
        -- ** Utility functions for printing and dumping Module objects
       ,moduleDump
        -- * Generic Value Accessors
       ,moduleGetNamedValue
       ,moduleGetTypeByName
        -- * Named Metadata Accessors
       ,moduleGetNamedMetadata
       ,moduleGetOrInsertNamedMetadata
       ,moduleEraseNamedMetadata
        -- * Globals list, functions list, and symbol table
       ,moduleGetGlobalList
       ,moduleGetFunctionList
       ,moduleGetNamedMDList
       ,parseIR
       ,writeBitCodeToFile
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.StringRef

import Foreign
import Foreign.C

writeBitCodeToFile :: Ptr Module -> String -> IO Bool
writeBitCodeToFile md name = withCString name
                             $ \str -> do
                               res <- writeBitCodeToFile_ md str
                               return $ res==0

moduleGetFunctionString :: Ptr Module -> String -> IO (Ptr Function)
moduleGetFunctionString md name
  = withStringRef name (moduleGetFunction md)

foreign import capi "extra.h writeBitCodeToFile"
  writeBitCodeToFile_ :: Ptr Module -> CString -> IO CInt
