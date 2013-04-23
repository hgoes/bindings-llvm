module LLVM.FFI.Pass.Haskell
       (HaskellModulePass()
       ,newHaskellModulePass
       ,deleteHaskellModulePass
       ,HaskellFunctionPass()
       ,newHaskellFunctionPass
       ,deleteHaskellFunctionPass
       ) where

import LLVM.FFI.Pass
import LLVM.FFI.Module
import LLVM.FFI.Function
import Foreign.Ptr

data HaskellModulePass = HaskellModulePass

data HaskellFunctionPass = HaskellFunctionPass

instance PassC HaskellModulePass
instance ModulePassC HaskellModulePass

instance PassC HaskellFunctionPass
instance FunctionPassC HaskellFunctionPass

newHaskellModulePass :: (Ptr HaskellModulePass -> Ptr AnalysisUsage -> IO ()) 
                        -> (Ptr HaskellModulePass -> Ptr Module -> IO Bool)
                        -> IO (Ptr HaskellModulePass)
newHaskellModulePass usage run = do
  usage_f <- wrap_AnalysisUsage_Void usage
  run_f <- wrap_Module_Bool run
  pass <- newHaskellModulePass_ usage_f run_f
  return pass

newHaskellFunctionPass :: (Ptr HaskellFunctionPass -> Ptr AnalysisUsage -> IO ())
                          -> (Ptr HaskellFunctionPass -> Ptr Module -> IO Bool)
                          -> (Ptr HaskellFunctionPass -> Ptr Module -> IO Bool)
                          -> (Ptr HaskellFunctionPass -> Ptr Function -> IO Bool)
                          -> IO (Ptr HaskellFunctionPass)
newHaskellFunctionPass usage init fin run = do
  usage_f <- wrap_AnalysisUsage_Void usage
  init_f <- wrap_Module_Bool init
  fin_f <- wrap_Module_Bool fin
  run_f <- wrap_Function_Bool run
  pass <- newHaskellFunctionPass_ usage_f init_f fin_f run_f
  return pass

foreign import capi "HaskellPass.h newHaskellModulePass"
  newHaskellModulePass_ :: FunPtr (Ptr HaskellModulePass -> Ptr AnalysisUsage -> IO ())
                           -> FunPtr (Ptr HaskellModulePass -> Ptr Module -> IO Bool)
                           -> IO (Ptr HaskellModulePass)

foreign import capi "HaskellPass.h newHaskellFunctionPass"
  newHaskellFunctionPass_ :: FunPtr (Ptr HaskellFunctionPass -> Ptr AnalysisUsage -> IO ())
                             -> FunPtr (Ptr HaskellFunctionPass -> Ptr Module -> IO Bool)
                             -> FunPtr (Ptr HaskellFunctionPass -> Ptr Module -> IO Bool)
                             -> FunPtr (Ptr HaskellFunctionPass -> Ptr Function -> IO Bool)
                             -> IO (Ptr HaskellFunctionPass)

foreign import capi "HaskellPass.h deleteHaskellModulePass"
  deleteHaskellModulePass :: Ptr HaskellModulePass -> IO ()

foreign import capi "HaskellPass.h deleteHaskellFunctionPass"
  deleteHaskellFunctionPass :: Ptr HaskellFunctionPass -> IO ()

foreign import ccall "wrapper"
  wrap_Module_Bool :: (Ptr a -> Ptr Module -> IO Bool) -> IO (FunPtr (Ptr a -> Ptr Module -> IO Bool))

foreign import ccall "wrapper"
  wrap_Function_Bool :: (Ptr a -> Ptr Function -> IO Bool) -> IO (FunPtr (Ptr a -> Ptr Function -> IO Bool))

foreign import ccall "wrapper"
  wrap_AnalysisUsage_Void :: (Ptr a -> Ptr AnalysisUsage -> IO ()) -> IO (FunPtr (Ptr a -> Ptr AnalysisUsage -> IO ()))