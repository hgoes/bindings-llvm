module LLVM.FFI.CPP.String
       (CPPString()
       ,cppStringEmpty
       ,cppStringFromString
       ,cppStringDelete
       ,cppStringToString
       ) where

import Data.Typeable
import Foreign.Ptr
import Foreign.C.String

data CPPString = CPPString deriving Typeable

foreign import capi "wrapper/extra.h std_string_empty"
  cppStringEmpty :: IO (Ptr CPPString)

foreign import capi "wrapper/extra.h std_string_from_string"
  cppStringFromString :: CString -> IO (Ptr CPPString)

foreign import capi "wrapper/extra.h std_string_delete"
  cppStringDelete :: Ptr CPPString -> IO ()

foreign import capi "wrapper/extra.h std_string_to_string"
  cppStringToString :: Ptr CPPString -> IO CString
