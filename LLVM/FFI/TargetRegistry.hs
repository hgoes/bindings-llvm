module LLVM.FFI.TargetRegistry
       (
#if HS_LLVM_VERSION>=300         
         targetRegistryPrintTargets,
#endif
         targetRegistryLookup
       ) where

import Foreign
import LLVM.FFI.Interface
import LLVM.FFI.CPP.String
import Foreign.C.String

targetRegistryLookup :: String -> IO (Either String (Ptr Target))
targetRegistryLookup name
  = withCString name $
    \cname -> do
      cppname <- cppStringFromString cname
      cpperr <- cppStringEmpty
      trg <- targetRegistryLookup_ cppname cpperr
      res <- if trg==nullPtr
             then (do
                      cerr <- cppStringToString cpperr
                      err <- peekCString cerr
                      return $ Left err)
             else return $ Right trg
      cppStringDelete cppname
      cppStringDelete cpperr
      return res
