module LLVM.FFI.Pass
       (Pass()
       ,PassC()
       ,PassId(..)
       ,deletePass
       ,passLookupPassInfo
       ,FunctionPass()
       ,FunctionPassC()
       ,functionPassRun
       ,ModulePass()
       ,ModulePassC()
       ,modulePassRunOnModule
       ,ImmutablePass()
       ,ImmutablePassC()
       ,AnalysisUsage()
       ,newAnalysisUsage
       ,analysisUsageAddRequired
       ,analysisUsageAddRequiredTransitive
       ,analysisUsageAddPreserved
       ,analysisUsagePreservesAll
       ,analysisUsagePreservesCFG
       ,FindUsedTypes()
       ,newFindUsedTypes
       ,deleteFindUsedTypes
       ,findUsedTypesGetTypes
       ,TargetLibraryInfo()
       ,LibFunc(..)
       ,newTargetLibraryInfo
       ,deleteTargetLibraryInfo
#if HS_LLVM_VERSION >= 303
       ,targetLibraryInfoGetLibFunc
#endif
#if HS_LLVM_VERSION >= 301
       ,targetLibraryInfoGetName
#endif
       ,targetLibraryInfoHas
#if HS_LLVM_VERSION >= 303
       ,DataLayout()
       ,newDataLayoutFromString
       ,newDataLayoutFromModule
       ,dataLayoutIsLittleEndian
       ,dataLayoutIsBigEndian
#else
       ,TargetData()
       ,newTargetDataFromString
       ,newTargetDataFromModule
       ,targetDataIsLittleEndian
       ,targetDataIsBigEndian
#endif
       ,LoopInfo()
       ,newLoopInfo
       ,loopInfoGetBase
       ,createCFGSimplificationPass
       ) where

import LLVM.FFI.Interface
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Proxy

#include "Helper.h"

class PassC t

class PassC t => PassId t where
  passId :: Proxy t -> Ptr CChar

instance PassC Pass
instance PassC FunctionPass
instance PassC ModulePass
instance PassC ImmutablePass
instance PassC FindUsedTypes
instance PassId FindUsedTypes where
  passId _ = passId_FindUsedTypes
instance PassC TargetLibraryInfo
instance PassId TargetLibraryInfo where
  passId _ = passId_TargetLibraryInfo
instance PassC LoopInfo
instance PassId LoopInfo where
  passId _ = passId_LoopInfo

class ModulePassC t

instance ModulePassC ModulePass
instance ModulePassC ImmutablePass
instance ModulePassC FindUsedTypes
instance ModulePassC TargetLibraryInfo

class ImmutablePassC t
instance ImmutablePassC TargetLibraryInfo

class FunctionPassC t
instance FunctionPassC FunctionPass
instance FunctionPassC LoopInfo

#if HS_LLVM_VERSION >= 303
instance PassC DataLayout
instance ModulePassC DataLayout
instance ImmutablePassC DataLayout
instance PassId DataLayout where
  passId _ = passId_DataLayout
foreign import capi _TO_STRING(extra.h passId_DataLayout)
  passId_DataLayout :: Ptr CChar
#else
instance PassC TargetData
instance ModulePassC TargetData
instance ImmutablePassC TargetData
instance PassId TargetData where
  passId _ = passId_TargetData
foreign import capi _TO_STRING(extra.h passId_TargetData)
  passId_TargetData :: Ptr CChar
#endif

deletePass :: PassC t => Ptr t -> IO ()
deletePass = deletePass_

modulePassRunOnModule :: ModulePassC p => Ptr p -> Ptr Module -> IO Bool
modulePassRunOnModule = modulePassRunOnModule_

functionPassRun :: FunctionPassC p => Ptr p -> Ptr Function -> IO Bool
functionPassRun = functionPassRun_

#if HS_LLVM_VERSION >= 303
targetLibraryInfoGetLibFunc :: Ptr TargetLibraryInfo -> Ptr StringRef -> IO (Maybe LibFunc)
targetLibraryInfoGetLibFunc tli str
  = alloca (\iptr -> do
               res <- targetLibraryInfoGetLibFunc_ tli str iptr
               if res
                 then (do
                          ires <- peek iptr
                          return (Just $ toLibFunc ires))
                 else return Nothing)
#endif

#if HS_LLVM_VERSION >= 301
targetLibraryInfoGetName :: Ptr TargetLibraryInfo -> LibFunc -> IO (Ptr StringRef)
targetLibraryInfoGetName ptr f = targetLibraryInfoGetName_ ptr (fromLibFunc f)
#endif

targetLibraryInfoHas :: Ptr TargetLibraryInfo -> LibFunc -> IO Bool
targetLibraryInfoHas ptr f = targetLibraryInfoHas_ ptr (fromLibFunc f)

data LibFunc =
#define HANDLE_LIBFUNC(name) PRESERVE(  ) Func_##name
#define HANDLE_SEP PRESERVE(  ) |
#include "LibFunc.def"
  deriving (Show,Eq,Ord)

toLibFunc :: CInt -> LibFunc
toLibFunc op
#define HANDLE_LIBFUNC(name) PRESERVE(  ) | op == libFunc_##name = Func_##name
#include "LibFunc.def"

fromLibFunc :: LibFunc -> CInt
#define HANDLE_LIBFUNC(name) fromLibFunc Func_##name = libFunc_##name
#include "LibFunc.def"

#define HANDLE_LIBFUNC(name) foreign import capi _TO_STRING(extra.h LibFunc_##name) libFunc_##name :: CInt
#include "LibFunc.def"

foreign import capi _TO_STRING(extra.h passId_LoopInfo)
  passId_LoopInfo :: Ptr CChar
foreign import capi _TO_STRING(extra.h passId_FindUsedTypes)
  passId_FindUsedTypes :: Ptr CChar
foreign import capi _TO_STRING(extra.h passId_TargetLibraryInfo)
  passId_TargetLibraryInfo :: Ptr CChar