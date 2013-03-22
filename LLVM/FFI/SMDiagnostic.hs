module LLVM.FFI.SMDiagnostic
       (SMDiagnostic(),
        newSMDiagnostic,
        deleteSMDiagnostic,
        getFilename,
        getLineNo,
        getColumnNo) where

import Foreign
import Foreign.C

import LLVM.FFI.Interface

getLineNo :: Ptr SMDiagnostic -> IO Integer
getLineNo ptr = fmap fromIntegral $ getLineNo_ ptr

getColumnNo :: Ptr SMDiagnostic -> IO Integer
getColumnNo ptr = fmap fromIntegral $ getColumnNo_ ptr