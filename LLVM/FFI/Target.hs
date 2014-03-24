module LLVM.FFI.Target
       (Target()
       ,targetNext
       ,targetName
       ,targetShortDescription
       ,targetHasJIT
       ,targetHasTargetMachine
#if HS_LLVM_VERSION<=303
       ,targetHasAsmPrinter
#if HS_LLVM_VERSION>=209
       ,targetHasAsmStreamer
#endif
#endif
#if HS_LLVM_VERSION>=300
       ,targetHasMCAsmBackend
#if HS_LLVM_VERSION<=303
       ,targetHasMCAsmParser
       ,targetHasMCDisassembler
       ,targetHasMCInstPrinter
       ,targetHasMCCodeEmitter
       ,targetHasMCObjectStreamer
#endif
#endif
       ) where

import LLVM.FFI.Interface
