module LLVM.FFI.Pass
       (Pass()
       ,PassC()
       ,PassId(..)
       ,PassKind(..)
       ,deletePass
       ,passGetKind
       ,passGetName
       ,passDump
       ,passGetAnalysis
       ,passGetResolver
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
       ,AnalysisResolver()
       ,analysisResolverFindImplPass
       ,analysisResolverFindImplPassFun
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
#if HS_LLVM_VERSION >= 302
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

passDump :: PassC t => Ptr t -> IO ()
passDump = passDump_

passGetName :: PassC t => Ptr t -> IO String
passGetName pass = passGetName_ pass >>= peekCString

passGetKind :: PassC t => Ptr t -> IO PassKind
passGetKind = fmap toPassKind . passGetKind_

passGetResolver :: PassC t => Ptr t -> IO (Ptr AnalysisResolver)
passGetResolver = passGetResolver_

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

#if HS_LLVM_VERSION >= 302
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

passGetAnalysis :: (PassC t,PassId analysis) => Ptr t -> IO (Ptr analysis)
passGetAnalysis pass = get Proxy
  where
    get :: PassId a => Proxy a -> IO (Ptr a)
    get p = do
      ptr <- passGetAdjustedAnalysisPointer_ pass (castPtr $ passId p)
      return (castPtr ptr)

modulePassRunOnModule :: ModulePassC p => Ptr p -> Ptr Module -> IO Bool
modulePassRunOnModule = modulePassRunOnModule_

functionPassRun :: FunctionPassC p => Ptr p -> Ptr Function -> IO Bool
functionPassRun = functionPassRun_

analysisUsageAddRequired :: PassId p => Ptr AnalysisUsage -> Proxy p -> IO ()
analysisUsageAddRequired au p = analysisUsageAddRequired_ au (passId p)

analysisUsageAddRequiredTransitive :: PassId p => Ptr AnalysisUsage -> Proxy p -> IO ()
analysisUsageAddRequiredTransitive au p = analysisUsageAddRequiredTransitive_ au (passId p)

analysisUsageAddPreserved :: PassId p => Ptr AnalysisUsage -> Proxy p -> IO ()
analysisUsageAddPreserved au p = analysisUsageAddPreserved_ au (passId p)

analysisResolverFindImplPass :: PassId p => Ptr AnalysisResolver -> Proxy p -> IO (Ptr Pass)
analysisResolverFindImplPass res p = analysisResolverFindImplPass_ res (castPtr $ passId p)

analysisResolverFindImplPassFun :: (PassC t,PassId p) => Ptr AnalysisResolver
                                   -> Ptr t -> Proxy p -> Ptr Function -> IO (Ptr Pass)
analysisResolverFindImplPassFun res pass p fun
  = analysisResolverFindImplPassFun_ res pass (castPtr $ passId p) fun

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

foreign import capi _TO_STRING(extra.h passId_LoopInfo)
  passId_LoopInfo :: Ptr CChar
foreign import capi _TO_STRING(extra.h passId_FindUsedTypes)
  passId_FindUsedTypes :: Ptr CChar
foreign import capi _TO_STRING(extra.h passId_TargetLibraryInfo)
  passId_TargetLibraryInfo :: Ptr CChar