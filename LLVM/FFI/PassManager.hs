module LLVM.FFI.PassManager
       (PassManager()
       ,newPassManager
       ,deletePassManager
       ,passManagerAdd
       ,passManagerRun
       ,FunctionPassManager()
       ,newFunctionPassManager
       ,deleteFunctionPassManager
       ,functionPassManagerAdd
       ,functionPassManagerRun
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Pass

import Foreign

passManagerAdd :: PassC t => Ptr PassManager -> Ptr t -> IO ()
passManagerAdd = passManagerAdd_

functionPassManagerAdd :: PassC t => Ptr FunctionPassManager -> Ptr t -> IO ()
functionPassManagerAdd = functionPassManagerAdd_