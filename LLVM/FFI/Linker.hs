module LLVM.FFI.Linker
       (Linker()
       ,newLinker
       ,deleteLinker
#if HS_LLVM_VERSION < 308
       ,linkerGetModule
#endif
       ,linkerLinkInModule)
       where

import LLVM.FFI.Interface
