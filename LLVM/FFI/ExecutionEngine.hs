module LLVM.FFI.ExecutionEngine
       (ExecutionEngine()
       ,ExecutionEngineC()
       ,deleteExecutionEngine
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
       ,executionEngineGetPointerToBasicBlock
       ,executionEngineGetPointerToFunctionOrStub
       ,executionEngineRunJITOnFunction
       ,executionEngineGetGlobalValueAtAddress
       ,executionEngineStoreValueToMemory
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Constant
import LLVM.FFI.Type
import Foreign.Ptr
import Foreign.C
import Data.Word

class ExecutionEngineC t

instance ExecutionEngineC ExecutionEngine

deleteExecutionEngine :: ExecutionEngineC t => Ptr t -> IO ()
deleteExecutionEngine = deleteExecutionEngine_

executionEngineAddModule :: ExecutionEngineC t => Ptr t -> Ptr Module -> IO ()
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

executionEngineUpdateGlobalMapping :: (ExecutionEngineC t,GlobalValueC v) => Ptr t -> Ptr v -> Ptr () -> IO (Ptr ())
executionEngineUpdateGlobalMapping = executionEngineUpdateGlobalMapping_

executionEngineGetPointerToBasicBlock :: ExecutionEngineC t => Ptr t -> Ptr BasicBlock -> IO (Ptr ())
executionEngineGetPointerToBasicBlock = executionEngineGetPointerToBasicBlock_

executionEngineRunJITOnFunction :: ExecutionEngineC t => Ptr t -> Ptr Function -> Ptr MachineCodeInfo -> IO ()
executionEngineRunJITOnFunction = executionEngineRunJITOnFunction_

executionEngineGetGlobalValueAtAddress :: ExecutionEngineC t => Ptr t -> Ptr () -> IO (Ptr GlobalValue)
executionEngineGetGlobalValueAtAddress = executionEngineGetGlobalValueAtAddress_

executionEngineStoreValueToMemory :: (ExecutionEngineC t,TypeC tp) => Ptr t -> Ptr GenericValue -> Ptr GenericValue -> Ptr tp -> IO ()
executionEngineStoreValueToMemory = executionEngineStoreValueToMemory_