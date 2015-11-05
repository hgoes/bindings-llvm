module LLVM.FFI.ExecutionEngine
       (ExecutionEngine()
       ,ExecutionEngineC()
       ,deleteExecutionEngine
#if HS_LLVM_VERSION<306
       ,newExecutionEngine
#endif
       ,executionEngineAddModule
       ,executionEngineRemoveModule
#if HS_LLVM_VERSION >= 302
       ,executionEngineGetDataLayout
#else
       ,executionEngineGetTargetData
#endif
       ,executionEngineFindFunctionNamed
       ,executionEngineRunFunction
#if HS_LLVM_VERSION >= 301
       ,executionEngineGetPointerToNamedFunction
       ,executionEngineMapSectionAddress
#endif
       ,executionEngineRunStaticConstructorsDestructors
       ,executionEngineAddGlobalMapping
       ,executionEngineClearAllGlobalMappings
       ,executionEngineUpdateGlobalMapping
       ,executionEngineGetPointerToGlobal
       ,executionEngineGetPointerToGlobalIfAvailable
       ,executionEngineGetPointerToFunction
#if HS_LLVM_VERSION<306
       ,executionEngineGetPointerToBasicBlock
#endif
       ,executionEngineGetPointerToFunctionOrStub
#if HS_LLVM_VERSION<306
       ,executionEngineRunJITOnFunction
#endif
       ,executionEngineGetGlobalValueAtAddress
       ,executionEngineStoreValueToMemory
       ,executionEngineInitializeMemory
#if HS_LLVM_VERSION<306
       ,executionEngineRecompileAndRelinkFunction
       ,executionEngineFreeMachineCodeForFunction
#endif
       ,executionEngineGetOrEmitGlobalVariable
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Constant
import LLVM.FFI.Type
import LLVM.FFI.CPP.String
import Foreign.Ptr
import Foreign.C
import Data.Word
#if HS_LLVM_VERSION>=306
import LLVM.FFI.CPP.UniquePtr
#endif

class ExecutionEngineC t

instance ExecutionEngineC ExecutionEngine

#if HS_LLVM_VERSION<306
newExecutionEngine :: Ptr Module -> Bool -> Ptr CPPString -> CodeGenOptLevel -> Bool -> IO (Ptr ExecutionEngine)
newExecutionEngine mod forceInterp errs lvl arg = newExecutionEngine_ mod forceInterp errs (fromCodeGenOptLevel lvl) arg
#endif

deleteExecutionEngine :: ExecutionEngineC t => Ptr t -> IO ()
deleteExecutionEngine = deleteExecutionEngine_

#if HS_LLVM_VERSION<306
executionEngineAddModule :: ExecutionEngineC t => Ptr t -> Ptr Module -> IO ()
#else
executionEngineAddModule :: ExecutionEngineC t => Ptr t -> Ptr (Unique_ptr Module) -> IO ()
#endif
executionEngineAddModule = executionEngineAddModule_

executionEngineRemoveModule :: ExecutionEngineC t => Ptr t -> Ptr Module -> IO Bool
executionEngineRemoveModule = executionEngineRemoveModule_

#if HS_LLVM_VERSION >= 302
executionEngineGetDataLayout :: ExecutionEngineC t => Ptr t -> IO (Ptr DataLayout)
executionEngineGetDataLayout = executionEngineGetDataLayout_
#else
executionEngineGetTargetData :: ExecutionEngineC t => Ptr t -> IO (Ptr TargetData)
executionEngineGetTargetData = executionEngineGetTargetData_
#endif

executionEngineFindFunctionNamed :: ExecutionEngineC t => Ptr t -> String -> IO (Ptr Function)
executionEngineFindFunctionNamed ptr name
  = withCString name $ \cname -> executionEngineFindFunctionNamed_ ptr cname

executionEngineRunFunction :: ExecutionEngineC t => Ptr t -> Ptr Function -> Ptr (Vector GenericValue) -> IO (Ptr GenericValue)
executionEngineRunFunction = executionEngineRunFunction_

#if HS_LLVM_VERSION >= 301
executionEngineGetPointerToNamedFunction :: ExecutionEngineC t => Ptr t -> String -> Bool -> IO (Ptr ())
executionEngineGetPointerToNamedFunction engine str abort
  = withCString str
    (\cstr -> executionEngineGetPointerToNamedFunction_ engine cstr abort)

executionEngineMapSectionAddress :: ExecutionEngineC t => Ptr t -> Ptr () -> Word64 -> IO ()
executionEngineMapSectionAddress = executionEngineMapSectionAddress_
#endif

executionEngineRunStaticConstructorsDestructors :: ExecutionEngineC t => Ptr t -> Bool -> IO ()
executionEngineRunStaticConstructorsDestructors = executionEngineRunStaticConstructorsDestructors_

executionEngineGetPointerToFunction :: ExecutionEngineC t => Ptr t -> Ptr Function -> IO (Ptr ())
executionEngineGetPointerToFunction = executionEngineGetPointerToFunction_

executionEngineGetPointerToFunctionOrStub :: ExecutionEngineC t => Ptr t -> Ptr Function -> IO (Ptr ())
executionEngineGetPointerToFunctionOrStub = executionEngineGetPointerToFunctionOrStub_

executionEngineGetPointerToGlobal :: (ExecutionEngineC t,GlobalValueC v) => Ptr t -> Ptr v -> IO (Ptr ())
executionEngineGetPointerToGlobal = executionEngineGetPointerToGlobal_

executionEngineGetPointerToGlobalIfAvailable :: (ExecutionEngineC t,GlobalValueC v) => Ptr t -> Ptr v -> IO (Ptr ())
executionEngineGetPointerToGlobalIfAvailable = executionEngineGetPointerToGlobalIfAvailable_

executionEngineAddGlobalMapping :: (ExecutionEngineC t,GlobalValueC v) => Ptr t -> Ptr v -> Ptr () -> IO ()
executionEngineAddGlobalMapping = executionEngineAddGlobalMapping_

executionEngineClearAllGlobalMappings :: ExecutionEngineC t => Ptr t -> IO ()
executionEngineClearAllGlobalMappings = executionEngineClearAllGlobalMappings_

#if HS_LLVM_VERSION<307
executionEngineUpdateGlobalMapping :: (ExecutionEngineC t,GlobalValueC v) => Ptr t -> Ptr v -> Ptr () -> IO (Ptr ())
#else
executionEngineUpdateGlobalMapping :: (ExecutionEngineC t,GlobalValueC v) => Ptr t -> Ptr v -> Ptr () -> IO Word64
#endif
executionEngineUpdateGlobalMapping = executionEngineUpdateGlobalMapping_

#if HS_LLVM_VERSION<306
executionEngineGetPointerToBasicBlock :: ExecutionEngineC t => Ptr t -> Ptr BasicBlock -> IO (Ptr ())
executionEngineGetPointerToBasicBlock = executionEngineGetPointerToBasicBlock_

executionEngineRunJITOnFunction :: ExecutionEngineC t => Ptr t -> Ptr Function -> Ptr MachineCodeInfo -> IO ()
executionEngineRunJITOnFunction = executionEngineRunJITOnFunction_
#endif

executionEngineGetGlobalValueAtAddress :: ExecutionEngineC t => Ptr t -> Ptr () -> IO (Ptr GlobalValue)
executionEngineGetGlobalValueAtAddress = executionEngineGetGlobalValueAtAddress_

executionEngineStoreValueToMemory :: (ExecutionEngineC t,TypeC tp) => Ptr t -> Ptr GenericValue -> Ptr GenericValue -> Ptr tp -> IO ()
executionEngineStoreValueToMemory = executionEngineStoreValueToMemory_

executionEngineInitializeMemory :: (ExecutionEngineC t,ConstantC c) => Ptr t -> Ptr c -> Ptr () -> IO ()
executionEngineInitializeMemory = executionEngineInitializeMemory_

#if HS_LLVM_VERSION<306
executionEngineRecompileAndRelinkFunction :: (ExecutionEngineC t) => Ptr t -> Ptr Function -> IO (Ptr ())
executionEngineRecompileAndRelinkFunction = executionEngineRecompileAndRelinkFunction_

executionEngineFreeMachineCodeForFunction :: (ExecutionEngineC t) => Ptr t -> Ptr Function -> IO ()
executionEngineFreeMachineCodeForFunction = executionEngineFreeMachineCodeForFunction_
#endif

executionEngineGetOrEmitGlobalVariable :: (ExecutionEngineC t) => Ptr t -> Ptr GlobalVariable -> IO (Ptr ())
executionEngineGetOrEmitGlobalVariable = executionEngineGetOrEmitGlobalVariable_
