module LLVM.FFI.Target
       (Target()
       ,targetNext
       ,targetName
       ,targetShortDescription
       ,targetHasJIT
       ,targetHasTargetMachine
       ,targetHasAsmPrinter
#if HS_LLVM_VERSION>=209
       ,targetHasAsmStreamer
#endif
#if HS_LLVM_VERSION>=300
       ,targetHasMCAsmBackend
       ,targetHasMCAsmParser
       ,targetHasMCDisassembler
       ,targetHasMCInstPrinter
       ,targetHasMCCodeEmitter
       ,targetHasMCObjectStreamer
#endif
       ) where

import LLVM.FFI.Interface