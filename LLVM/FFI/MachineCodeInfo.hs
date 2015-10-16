module LLVM.FFI.MachineCodeInfo
#if HS_LLVM_VERSION<306
       (MachineCodeInfo()
       ,newMachineCodeInfo
       ,machineCodeInfoSetSize
       ,machineCodeInfoSetAddress
       ,machineCodeInfoGetSize
       ,machineCodeInfoGetAddress
       )
#endif
  where

import LLVM.FFI.Interface
