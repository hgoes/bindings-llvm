module LLVM.FFI.Pass
       (Pass()
       ,PassC()
       ,FunctionPass()
       ,ModulePass()
       ,ModulePassC()
       ,modulePassRunOnModule
       ,FindUsedTypes()
       ,newFindUsedTypes
       ,deleteFindUsedTypes
       ,findUsedTypesGetTypes
       ,createCFGSimplificationPass
       ,passLookupPassInfo
       
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr

class PassC t

instance PassC Pass
instance PassC FunctionPass
instance PassC ModulePass
instance PassC FindUsedTypes

class ModulePassC t

instance ModulePassC ModulePass
instance ModulePassC FindUsedTypes

modulePassRunOnModule :: ModulePassC p => Ptr p -> Ptr Module -> IO Bool
modulePassRunOnModule = modulePassRunOnModule_