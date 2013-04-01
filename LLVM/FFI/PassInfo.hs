module LLVM.FFI.PassInfo 
       (PassInfo()
       ,deletePassInfo
       ,passInfoGetPassName
       ,passInfoGetPassArgument
       ,passInfoCreatePass
       )
       where

import LLVM.FFI.Interface
import Foreign
import Foreign.C

passInfoGetPassName :: Ptr PassInfo -> IO String
passInfoGetPassName ptr = do
  str <- passInfoGetPassName_ ptr
  peekCString str

passInfoGetPassArgument :: Ptr PassInfo -> IO String
passInfoGetPassArgument ptr = do
  str <- passInfoGetPassArgument_ ptr
  peekCString str