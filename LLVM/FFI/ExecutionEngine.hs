module LLVM.FFI.ExecutionEngine
       (ExecutionEngine()
       ,ExecutionEngineC()
       ,deleteExecutionEngine
       ,executionEngineAddModule
       ,executionEngineDeleteModule
       ,executionEngineFindFunctionNamed
       ,executionEngineRunFunction
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr
import Foreign.C

class ExecutionEngineC t

instance ExecutionEngineC ExecutionEngine

deleteExecutionEngine :: ExecutionEngineC t => Ptr t -> IO ()
deleteExecutionEngine = deleteExecutionEngine_

executionEngineAddModule :: ExecutionEngineC t => Ptr t -> Ptr Module -> IO ()
executionEngineAddModule = executionEngineAddModule_

executionEngineDeleteModule :: ExecutionEngineC t => Ptr t -> Ptr Module -> IO Bool
executionEngineDeleteModule = executionEngineDeleteModule_

executionEngineFindFunctionNamed :: ExecutionEngineC t => Ptr t -> String -> IO (Ptr Function)
executionEngineFindFunctionNamed ptr name
  = withCString name $ \cname -> executionEngineFindFunctionNamed_ ptr cname

executionEngineRunFunction :: ExecutionEngineC t => Ptr t -> Ptr Function -> Ptr (Vector GenericValue) -> IO (Ptr GenericValue)
executionEngineRunFunction = executionEngineRunFunction_