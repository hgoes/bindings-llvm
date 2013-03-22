module LLVM.FFI.Context 
       (LLVMContext()
       ,newLLVMContext
       ,deleteLLVMContext
       ,withContext)
       where

import Foreign

import LLVM.FFI.Interface

withContext :: (Ptr LLVMContext -> IO a) -> IO a
withContext f = do
  ctx <- newLLVMContext
  res <- f ctx
  deleteLLVMContext ctx
  return res