module ExampleModule where

import LLVM.FFI
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

createExampleModule :: Ptr LLVMContext -> IO (Ptr Module)
createExampleModule ctx = do
  name <- newStringRef "example-module"
  fname <- newTwineString "add11"
  blkName <- newTwineString "start"
  argName <- newTwineString "arg1"
  binName <- newTwineString "res"
  mod <- newModule name ctx
  i32 <- getIntegerType ctx 32
  argArr <- mallocArray 1
  poke argArr (castUp i32)
#if HS_LLVM_VERSION>=300
  argArrRef <- newArrayRef' argArr 1
#else
  argArrRef <- newVector argArr (advancePtr argArr 1)
#endif
  funTp <- newFunctionType i32 argArrRef False
  fun <- createFunction funTp CommonLinkage fname mod
  blk <- createBasicBlock ctx blkName fun nullPtr
  instrs <- getInstList blk
  ap11 <- newAPIntLimited 32 11 False
  c11 <- createConstantInt ctx ap11
  deleteAPInt ap11
  arg1 <- createArgument i32 argName fun
  add <- newBinaryOperator Add c11 arg1 binName
  ipListPushBack instrs (castUp add)
  ret <- newReturnInst ctx add
  ipListPushBack instrs (castUp ret)
  return mod

testExampleModule :: IO ()
testExampleModule
  = withContext $ \ctx -> do
    mod <- createExampleModule ctx
    moduleDump mod
