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
#if HS_LLVM_VERSION <= 303
       ,setPassManagerBuilderDisableSimplifyLibCalls
#endif
       ,setPassManagerBuilderDisableUnitAtATime
       ,setPassManagerBuilderDisableUnrollLoops
#if HS_LLVM_VERSION >= 301
#if HS_LLVM_VERSION >= 303
       ,setPassManagerBuilderBBVectorize
       ,setPassManagerBuilderSLPVectorize
#else
       ,setPassManagerBuilderVectorize
#if HS_LLVM_VERSION >= 302
       ,setPassManagerBuilderLoopVectorize
#endif
#endif
#endif
#endif
       ) where

import LLVM.FFI.Interface
