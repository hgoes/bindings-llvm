module LLVM.FFI.Pass
       (Pass()
       ,PassC()
       ,passLookupPassInfo
       ,FunctionPass()
       ,ModulePass()
       ,ModulePassC()
       ,modulePassRunOnModule
       ,ImmutablePass()
       ,ImmutablePassC()
       ,FindUsedTypes()
       ,newFindUsedTypes
       ,deleteFindUsedTypes
       ,findUsedTypesGetTypes
       ,TargetLibraryInfo()
       ,newTargetLibraryInfo
       ,deleteTargetLibraryInfo
       ,createCFGSimplificationPass
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr

class PassC t

instance PassC Pass
instance PassC FunctionPass
instance PassC ModulePass
instance PassC ImmutablePass
instance PassC FindUsedTypes
instance PassC TargetLibraryInfo

class ModulePassC t

instance ModulePassC ModulePass
instance ModulePassC ImmutablePass
instance ModulePassC FindUsedTypes
instance ModulePassC TargetLibraryInfo

class ImmutablePassC t
instance ImmutablePassC TargetLibraryInfo

modulePassRunOnModule :: ModulePassC p => Ptr p -> Ptr Module -> IO Bool
modulePassRunOnModule = modulePassRunOnModule_