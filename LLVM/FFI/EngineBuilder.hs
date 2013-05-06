module LLVM.FFI.EngineBuilder
       (EngineBuilder()
       ,EngineKind(..)
       ,CodeGenOptLevel(..)
       ,CodeModel(..)
       ,RelocModel(..)
       ,newEngineBuilder
       ,deleteEngineBuilder
       ,engineBuilderSetKind
       ,engineBuilderSetOptLevel
#if HS_LLVM_VERSION>=300
       ,engineBuilderSetRelocationModel
#endif
       ,engineBuilderSetCodeModel
       ,engineBuilderCreate
       ) where

import LLVM.FFI.Interface