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
       ,setPassManagerBuilderVectorize
       --,setPassManagerBuilderLoopVectorize
       ) where

import LLVM.FFI.Interface