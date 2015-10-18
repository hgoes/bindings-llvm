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
       ,BasicBlockPass()
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
#if HS_LLVM_VERSION < 306
       ,FindUsedTypes()
       ,newFindUsedTypes
       ,deleteFindUsedTypes
       ,findUsedTypesGetTypes
#endif
#if HS_LLVM_VERSION >= 209
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
#endif
#if HS_LLVM_VERSION >= 302
       ,DataLayout()
#if HS_LLVM_VERSION >= 305
       ,DataLayoutPass()
       ,dataLayoutPassGetDataLayout
#endif
       ,newDataLayoutFromString
       ,newDataLayoutFromModule
       ,dataLayoutIsLittleEndian
       ,dataLayoutIsBigEndian
       ,dataLayoutIsLegalInteger
       ,dataLayoutExceedsNaturalStackAlignment
       ,dataLayoutFitsInLegalInteger
       ,dataLayoutPointerABIAlignment
       ,dataLayoutPointerPrefAlignment
       ,dataLayoutPointerSize
       ,dataLayoutTypeSizeInBits
       ,dataLayoutTypeStoreSize
       ,dataLayoutTypeAllocSize
       ,dataLayoutABITypeAlignment
       ,dataLayoutABIIntegerTypeAlignment
#if HS_LLVM_VERSION<305
       ,dataLayoutCallFrameTypeAlignment
#endif
       ,dataLayoutPrefTypeAlignment
       ,dataLayoutIntPtrType
       ,dataLayoutIntPtrTypeForType
       ,dataLayoutStructLayout
       ,dataLayoutPreferedAlignment
#else
       ,TargetData()
       ,newTargetDataFromString
       ,newTargetDataFromModule
       ,targetDataIsLittleEndian
       ,targetDataIsBigEndian
       ,targetDataIsLegalInteger
#if HS_LLVM_VERSION >= 300
       ,targetDataExceedsNaturalStackAlignment
       ,targetDataFitsInLegalInteger
#endif
       ,targetDataPointerABIAlignment
       ,targetDataPointerPrefAlignment
       ,targetDataPointerSize
       ,targetDataTypeSizeInBits
       ,targetDataTypeStoreSize
       ,targetDataTypeAllocSize
       ,targetDataABITypeAlignment
       ,targetDataABIIntegerTypeAlignment
       ,targetDataCallFrameTypeAlignment
       ,targetDataPrefTypeAlignment
       ,targetDataIntPtrType
       ,targetDataStructLayout
       ,targetDataPreferedAlignment
#endif
       ,StructLayout()
       ,structLayoutSizeInBytes
       ,structLayoutSizeInBits
       ,structLayoutAlignment
       ,structLayoutElementContainingOffset
       ,structLayoutElementOffset
       ,structLayoutElementOffsetInBits
       ,LoopInfo()
       ,newLoopInfo
       ,loopInfoGetBase
       ,createCFGSimplificationPass
       ,DominatorTree()
       ,DomTreeNodeBase()
       ,DomTreeNodeBaseC(..)
       ,newDominatorTree
       ,deleteDominatorTree
#if HS_LLVM_VERSION>=305
       ,dominatorTreeRecalculate
#endif
       ,dominatorTreeGetRootNode
       ,dominatorTreeCompare
       ,dominatorTreeDominates
       ,dominatorTreeFindNearestCommonDominator
       ,dominatorTreeGetNode
#if HS_LLVM_VERSION>=305
       ,DominatorTreeWrapperPass()
       ,dominatorTreeWrapperPassGetDomTree
#endif
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.CPP
import LLVM.FFI.Type
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Proxy
import Data.Word

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

class PassId t where
  passId :: Proxy t -> Ptr CChar

instance PassC Pass
instance PassC FunctionPass
instance PassC ModulePass
instance PassC ImmutablePass
instance PassC BasicBlockPass
#if HS_LLVM_VERSION<306
instance PassC FindUsedTypes
instance PassId FindUsedTypes where
  passId _ = passId_FindUsedTypes
#endif
#if HS_LLVM_VERSION>=209
instance PassC TargetLibraryInfo
instance PassId TargetLibraryInfo where
  passId _ = passId_TargetLibraryInfo
#endif
instance PassC LoopInfo
instance PassId LoopInfo where
  passId _ = passId_LoopInfo

class ModulePassC t

instance ModulePassC ModulePass
instance ModulePassC ImmutablePass
#if HS_LLVM_VERSION<306
instance ModulePassC FindUsedTypes
#endif
#if HS_LLVM_VERSION>=209
instance ModulePassC TargetLibraryInfo
#endif

class ImmutablePassC t
#if HS_LLVM_VERSION>=209
instance ImmutablePassC TargetLibraryInfo
#endif

class FunctionPassC t
instance FunctionPassC FunctionPass
instance FunctionPassC LoopInfo

#if HS_LLVM_VERSION < 302
instance PassC TargetData
instance ModulePassC TargetData
instance ImmutablePassC TargetData
instance PassId TargetData where
  passId _ = passId_TargetData
foreign import capi _TO_STRING(extra.h passId_TargetData)
  passId_TargetData :: Ptr CChar
#elif HS_LLVM_VERSION < 305
instance PassC DataLayout
instance ModulePassC DataLayout
instance ImmutablePassC DataLayout
instance PassId DataLayout where
  passId _ = passId_DataLayout
foreign import capi _TO_STRING(extra.h passId_DataLayout)
  passId_DataLayout :: Ptr CChar
#else
instance PassC DataLayoutPass
instance ModulePassC DataLayoutPass
instance ImmutablePassC DataLayoutPass
instance PassId DataLayoutPass where
  passId _ = passId_DataLayoutPass
foreign import capi _TO_STRING(extra.h passId_DataLayoutPass)
  passId_DataLayoutPass :: Ptr CChar
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
analysisUsageAddRequired au p = analysisUsageAddRequired_ au (castPtr $ passId p)

analysisUsageAddRequiredTransitive :: PassId p => Ptr AnalysisUsage -> Proxy p -> IO ()
analysisUsageAddRequiredTransitive au p = analysisUsageAddRequiredTransitive_ au (castPtr $ passId p)

analysisUsageAddPreserved :: PassId p => Ptr AnalysisUsage -> Proxy p -> IO ()
analysisUsageAddPreserved au p = analysisUsageAddPreserved_ au (castPtr $ passId p)

analysisResolverFindImplPass :: PassId p => Ptr AnalysisResolver -> Proxy p -> IO (Ptr Pass)
analysisResolverFindImplPass res p = analysisResolverFindImplPass_ res (castPtr $ passId p)

analysisResolverFindImplPassFun :: (PassC t,PassId p) => Ptr AnalysisResolver
                                   -> Ptr t -> Proxy p -> Ptr Function -> IO (Ptr Pass)
analysisResolverFindImplPassFun res pass p fun
  = analysisResolverFindImplPassFun_ res pass (castPtr $ passId p) fun

#if HS_LLVM_VERSION >= 209
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

foreign import capi _TO_STRING(extra.h passId_TargetLibraryInfo)
  passId_TargetLibraryInfo :: Ptr CChar
#endif

foreign import capi _TO_STRING(extra.h passId_LoopInfo)
  passId_LoopInfo :: Ptr CChar
#if HS_LLVM_VERSION<306
foreign import capi _TO_STRING(extra.h value passId_FindUsedTypes)
  passId_FindUsedTypes :: Ptr CChar
#endif

#if HS_LLVM_VERSION<305
foreign import capi _TO_STRING(extra.h passId_DominatorTree)
  passId_DominatorTree :: Ptr CChar

instance PassC DominatorTree
instance FunctionPassC DominatorTree
instance PassId DominatorTree where
  passId _ = passId_DominatorTree
#else
foreign import capi _TO_STRING(extra.h passId_DominatorTreeWrapperPass)
  passId_DominatorTreeWrapperPass :: Ptr CChar

instance PassC DominatorTreeWrapperPass
instance FunctionPassC DominatorTreeWrapperPass
instance PassId DominatorTreeWrapperPass where
  passId _ = passId_DominatorTreeWrapperPass
#endif

class DomTreeNodeBaseC tp where
  domTreeNodeBaseGetBlock :: Ptr (DomTreeNodeBase tp) -> IO (Ptr tp)
  domTreeNodeBaseGetIDom :: Ptr (DomTreeNodeBase tp) -> IO (Ptr (DomTreeNodeBase tp))
  domTreeNodeBaseGetChildren :: Ptr (DomTreeNodeBase tp) -> IO (Ptr (Vector (Ptr (DomTreeNodeBase tp))))
  domTreeNodeBaseCompare :: Ptr (DomTreeNodeBase tp) -> Ptr (DomTreeNodeBase tp) -> IO Bool
  domTreeNodeBaseGetDFSNumIn :: Ptr (DomTreeNodeBase tp) -> IO CUInt
  domTreeNodeBaseGetDFSNumOut :: Ptr (DomTreeNodeBase tp) -> IO CUInt

instance DomTreeNodeBaseC BasicBlock where
  domTreeNodeBaseGetBlock = domTreeNodeBaseGetBlockBasicBlock
  domTreeNodeBaseGetIDom = domTreeNodeBaseGetIDomBasicBlock
  domTreeNodeBaseGetChildren = domTreeNodeBaseGetChildrenBasicBlock
  domTreeNodeBaseCompare = domTreeNodeBaseCompareBasicBlock
  domTreeNodeBaseGetDFSNumIn = domTreeNodeBaseGetDFSNumInBasicBlock
  domTreeNodeBaseGetDFSNumOut = domTreeNodeBaseGetDFSNumOutBasicBlock

instance VectorC (Ptr (DomTreeNodeBase BasicBlock)) where
  vectorBegin = vectorDominatorTreeBegin
  vectorEnd = vectorDominatorTreeEnd
  vectorIteratorDeref = vectorIteratorDominatorTreeDeref
  vectorIteratorNext = vectorIteratorDominatorTreeNext
  vectorIteratorEq = vectorIteratorDominatorTreeEq

#if HS_LLVM_VERSION >= 302
dataLayoutTypeSizeInBits :: TypeC tp => Ptr DataLayout -> Ptr tp -> IO Word64
dataLayoutTypeSizeInBits = dataLayoutTypeSizeInBits_

dataLayoutTypeStoreSize :: TypeC tp => Ptr DataLayout -> Ptr tp -> IO Word64
dataLayoutTypeStoreSize = dataLayoutTypeStoreSize_

dataLayoutTypeAllocSize :: TypeC tp => Ptr DataLayout -> Ptr tp -> IO Word64
dataLayoutTypeAllocSize = dataLayoutTypeAllocSize_

dataLayoutABITypeAlignment :: TypeC tp => Ptr DataLayout -> Ptr tp -> IO CUInt
dataLayoutABITypeAlignment = dataLayoutABITypeAlignment_

#if HS_LLVM_VERSION<305
dataLayoutCallFrameTypeAlignment :: TypeC tp => Ptr DataLayout -> Ptr tp -> IO CUInt
dataLayoutCallFrameTypeAlignment = dataLayoutCallFrameTypeAlignment_
#endif

dataLayoutPrefTypeAlignment :: TypeC tp => Ptr DataLayout -> Ptr tp -> IO CUInt
dataLayoutPrefTypeAlignment = dataLayoutPrefTypeAlignment_

dataLayoutIntPtrTypeForType :: TypeC tp => Ptr DataLayout -> Ptr tp -> IO (Ptr Type)
dataLayoutIntPtrTypeForType = dataLayoutIntPtrTypeForType_
#else
targetDataTypeSizeInBits :: TypeC tp => Ptr TargetData -> Ptr tp -> IO Word64
targetDataTypeSizeInBits = targetDataTypeSizeInBits_

targetDataTypeStoreSize :: TypeC tp => Ptr TargetData -> Ptr tp -> IO Word64
targetDataTypeStoreSize = targetDataTypeStoreSize_

targetDataTypeAllocSize :: TypeC tp => Ptr TargetData -> Ptr tp -> IO Word64
targetDataTypeAllocSize = targetDataTypeAllocSize_

targetDataABITypeAlignment :: TypeC tp => Ptr TargetData -> Ptr tp -> IO CUInt
targetDataABITypeAlignment = targetDataABITypeAlignment_

targetDataCallFrameTypeAlignment :: TypeC tp => Ptr TargetData -> Ptr tp -> IO CUInt
targetDataCallFrameTypeAlignment = targetDataCallFrameTypeAlignment_

targetDataPrefTypeAlignment :: TypeC tp => Ptr TargetData -> Ptr tp -> IO CUInt
targetDataPrefTypeAlignment = targetDataPrefTypeAlignment_
#endif
