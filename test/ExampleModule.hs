module ExampleModule where

import LLVM.FFI
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

createExampleModule :: Ptr LLVMContext -> IO (Ptr Module,Ptr Function)
createExampleModule ctx = do
  name <- newStringRef "example-module"
  --fname <- newTwineString "add11"
  fname <- newStringRef "add11"
  withTwine "start" $
    \blkName -> withTwine "arg1" $
    \argName -> withTwine "res" $
    \binName -> do
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
#if HS_LLVM_VERSION>=303
      attrs <- newAttributeSet
#else
      attrs <- newAttrListPtr
#endif
      --fun <- createFunction funTp CommonLinkage fname mod
      fun' <- moduleGetOrInsertFunction mod fname funTp attrs
      let Just fun = castDown fun'
      blk <- createBasicBlock ctx blkName fun nullPtr
      instrs <- getInstList blk
      ap11 <- newAPIntLimited 32 11 False
      c11 <- createConstantInt ctx ap11
      deleteAPInt ap11
      argList <- functionGetArgumentList fun
      [arg1] <- ipListToList argList
      add <- newBinaryOperator Add c11 arg1 binName
      ipListPushBack instrs (castUp add)
      ret <- newReturnInst ctx add
      ipListPushBack instrs (castUp ret)
      return (mod,fun)

testExampleModule :: IO ()
testExampleModule
  = withContext $ \ctx -> do
    (mod,_) <- createExampleModule ctx
    moduleDump mod
