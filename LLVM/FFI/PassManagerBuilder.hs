module LLVM.FFI.PassManagerBuilder
       (PassManagerBuilder()
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
       --,setPassManagerBuilderLoopVectorize
       ) where

import LLVM.FFI.Interface