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
       ,engineBuilderSetErrorStr
       ,engineBuilderCreate
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr

engineBuilderSetKind :: Ptr EngineBuilder -> EngineKind -> IO ()
engineBuilderSetKind builder kind
  = engineBuilderSetKind_ builder (fromEngineKind kind)

engineBuilderSetOptLevel :: Ptr EngineBuilder -> CodeGenOptLevel -> IO ()
engineBuilderSetOptLevel builder optlevel
  = engineBuilderSetOptLevel_ builder (fromCodeGenOptLevel optlevel)

#if HS_LLVM_VERSION>=300
engineBuilderSetRelocationModel :: Ptr EngineBuilder -> RelocModel -> IO ()
engineBuilderSetRelocationModel builder reloc
  = engineBuilderSetRelocationModel_ builder (fromRelocModel reloc)
#endif

engineBuilderSetCodeModel :: Ptr EngineBuilder -> CodeModel -> IO ()
engineBuilderSetCodeModel builder cm
  = engineBuilderSetCodeModel_ builder (fromCodeModel cm)
