module LLVM.FFI.PassRegistry
       (PassRegistry()
       ,passRegistryGet
       ,newPassRegistry
       ,deletePassRegistry
       ,passRegistryGetPassInfo
       ,passRegistryGetPassInfoByName
       ,passRegistryRegisterPass
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Pass

import Data.Proxy
import Foreign.Ptr

passRegistryGetPassInfo :: PassId p => Ptr PassRegistry -> Proxy p -> IO (Ptr PassInfo)
passRegistryGetPassInfo reg prx
  = passRegistryGetPassInfo_ reg (castPtr $ passId prx)
