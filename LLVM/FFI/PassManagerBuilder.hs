module LLVM.FFI.PassManagerBuilder
       (
#if HS_LLVM_VERSION >= 300
         PassManagerBuilder()
       ,newPassManagerBuilder
       ,deletePassManagerBuilder
       ,populateFunctionPassManager
       ,setPassManagerBuilderOptLevel
       ,setPassManagerBuilderSizeLevel
       ,setPassManagerBuilderInliner
       ,setPassManagerBuilderDisableSimplifyLibCalls
       ,setPassManagerBuilderDisableUnitAtATime
       ,setPassManagerBuilderDisableUnrollLoops
#if HS_LLVM_VERSION >= 301
       ,setPassManagerBuilderVectorize
#endif
#endif
       ) where

import LLVM.FFI.Interface