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
#if HS_LLVM_VERSION >= 303
       ,setPassManagerBuilderBBVectorize
       ,setPassManagerBuilderSLPVectorize
#else
       ,setPassManagerBuilderVectorize
#endif
       ,setPassManagerBuilderLoopVectorize
#endif
#endif
       ) where

import LLVM.FFI.Interface
