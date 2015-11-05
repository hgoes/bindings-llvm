module LLVM.FFI.AliasAnalysis
#if HS_LLVM_VERSION < 308
       (AliasAnalysis()
       ,AliasAnalysisC()
       ,newAliasAnalysis
       ,deleteAliasAnalysis
#if HS_LLVM_VERSION >= 303
       ,aliasAnalysisGetTargetLibraryInfo
#endif
#if HS_LLVM_VERSION<307
       ,aliasAnalysisGetTypeStoreSize
#if HS_LLVM_VERSION>=209
       ,aliasAnalysisGetLocationLoad
       ,aliasAnalysisGetLocationStore
       ,aliasAnalysisGetLocationVAArg
#endif
#if HS_LLVM_VERSION >= 300
       ,aliasAnalysisGetLocationAtomicCmpXchg
       ,aliasAnalysisGetLocationAtomicRMWInst
#endif
#endif
       ,aliasAnalysisAlias
#else
       (AAResults()
       ,AAResultsC()
       ,newAAResults
       ,deleteAAResults
       ,aaResultsGetTargetLibraryInfo
       ,aaResultsAlias
#endif
       ,AliasResult(..)
#if HS_LLVM_VERSION>=307
       ,MemoryLocation(..)
       ,newMemoryLocation
#elif HS_LLVM_VERSION>=209
       ,Location(..)
       ,newLocation
#endif
       ) where

import LLVM.FFI.Metadata()

import LLVM.FFI.Interface
import LLVM.FFI.Value
#if HS_LLVM_VERSION >= 303
import LLVM.FFI.Pass (TargetLibraryInfo)
#endif
import LLVM.FFI.Type (TypeC)
import LLVM.FFI.Pass (PassId(..))
#if HS_LLVM_VERSION >= 300
import LLVM.FFI.Instruction (LoadInst,StoreInst,VAArgInst,AtomicCmpXchgInst,AtomicRMWInst)
#else
import LLVM.FFI.Instruction (LoadInst,StoreInst,VAArgInst)
#endif
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word

#include "Helper.h"

#if HS_LLVM_VERSION<308
class AliasAnalysisC t
instance AliasAnalysisC AliasAnalysis

#if HS_LLVM_VERSION<307
instance PassId AliasAnalysis where
  passId _ = passId_AliasAnalysis
foreign import capi _TO_STRING(extra.h passId_AliasAnalysis)
  passId_AliasAnalysis :: Ptr CChar
#endif

deleteAliasAnalysis :: AliasAnalysisC t => Ptr t -> IO ()
deleteAliasAnalysis = deleteAliasAnalysis_

#if HS_LLVM_VERSION >= 303
aliasAnalysisGetTargetLibraryInfo :: AliasAnalysisC t => Ptr t -> IO (Ptr TargetLibraryInfo)
aliasAnalysisGetTargetLibraryInfo = aliasAnalysisGetTargetLibraryInfo_
#endif

#if HS_LLVM_VERSION<307
aliasAnalysisGetTypeStoreSize :: (AliasAnalysisC a,TypeC tp) => Ptr a -> Ptr tp -> IO Word64
aliasAnalysisGetTypeStoreSize = aliasAnalysisGetTypeStoreSize_

#if HS_LLVM_VERSION>=209
aliasAnalysisGetLocationLoad :: AliasAnalysisC t => Ptr t -> Ptr LoadInst -> IO (Ptr Location)
aliasAnalysisGetLocationLoad = aliasAnalysisGetLocationLoad_

aliasAnalysisGetLocationStore :: AliasAnalysisC t => Ptr t -> Ptr StoreInst -> IO (Ptr Location)
aliasAnalysisGetLocationStore = aliasAnalysisGetLocationStore_

aliasAnalysisGetLocationVAArg :: AliasAnalysisC t => Ptr t -> Ptr VAArgInst -> IO (Ptr Location)
aliasAnalysisGetLocationVAArg = aliasAnalysisGetLocationVAArg_
#endif

#if HS_LLVM_VERSION >= 300
aliasAnalysisGetLocationAtomicCmpXchg :: AliasAnalysisC t => Ptr t -> Ptr AtomicCmpXchgInst -> IO (Ptr Location)
aliasAnalysisGetLocationAtomicCmpXchg = aliasAnalysisGetLocationAtomicCmpXchg_

aliasAnalysisGetLocationAtomicRMWInst :: AliasAnalysisC t => Ptr t -> Ptr AtomicRMWInst -> IO (Ptr Location)
aliasAnalysisGetLocationAtomicRMWInst = aliasAnalysisGetLocationAtomicRMW_
#endif
#endif

#if HS_LLVM_VERSION>=209
#if HS_LLVM_VERSION<307
aliasAnalysisAlias :: AliasAnalysisC t => Ptr t -> Ptr Location -> Ptr Location -> IO AliasResult
#else
aliasAnalysisAlias :: AliasAnalysisC t => Ptr t
                   -> Ptr MemoryLocation -> Ptr MemoryLocation
                   -> IO AliasResult
#endif
aliasAnalysisAlias ptr l1 l2 = fmap toAliasResult $ aliasAnalysisAlias_ ptr l1 l2

#if HS_LLVM_VERSION<306
newLocation :: ValueC v => Ptr v -> Word64 -> Ptr MDNode -> IO (Ptr Location)
newLocation = newLocation_
#elif HS_LLVM_VERSION<307
newLocation :: ValueC v => Ptr v -> Word64 -> Ptr AAMDNodes -> IO (Ptr Location)
newLocation = newLocation_
#else
newMemoryLocation :: ValueC v => Ptr v -> Word64 -> Ptr AAMDNodes -> IO (Ptr MemoryLocation)
newMemoryLocation = newMemoryLocation_
#endif

#if HS_LLVM_VERSION<307
instance Storable Location where
  sizeOf _ = fromIntegral locationSizeOf
  alignment _ = fromIntegral locationAlignOf
  peek loc = do
    ptr <- locationGetPtr loc
    size <- locationGetSize loc
#if HS_LLVM_VERSION>305
    aatags <- locationGetAATags loc >>= peek
    return $ Location ptr size aatags
#else
    tbaatag <- locationGetTBAATag loc
    return $ Location ptr size tbaatag
#endif
  poke loc (Location ptr size aatags) = do
    locationSetPtr loc ptr
    locationSetSize loc size
#if HS_LLVM_VERSION>305
    alloca $ \aatags' -> do
      poke aatags' aatags
      locationSetAATags loc aatags'
#else
    locationSetTBAATag loc aatags
#endif
#else
instance Storable MemoryLocation where
  sizeOf _ = fromIntegral memoryLocationSizeOf
  alignment _ = fromIntegral memoryLocationAlignOf
  peek loc = do
    ptr <- memoryLocationGetPtr loc
    size <- memoryLocationGetSize loc
    aatags <- memoryLocationGetAATags loc >>= peek
    return $ MemoryLocation ptr size aatags
  poke loc (MemoryLocation ptr size aatags) = do
    memoryLocationSetPtr loc ptr
    memoryLocationSetSize loc size
    alloca $ \aatags' -> do
      poke aatags' aatags
      memoryLocationSetAATags loc aatags'
#endif
#else
aliasAnalysisAlias :: AliasAnalysisC t => Ptr t -> Ptr Value -> CUInt -> Ptr Value -> CUInt -> IO AliasResult
aliasAnalysisAlias ptr v1 l1 v2 l2 = fmap toAliasResult $ aliasAnalysisAlias_ ptr v1 l1 v2 l2
#endif

#else
-- HS_LLVM_VERSION>=308
class AAResultsC t
instance AAResultsC AAResults

deleteAAResults :: AAResultsC t => Ptr t -> IO ()
deleteAAResults = deleteAAResults_

aaResultsGetTargetLibraryInfo :: AAResultsC t => Ptr t -> IO (Ptr TargetLibraryInfo)
aaResultsGetTargetLibraryInfo = aaResultsGetTargetLibraryInfo_

aaResultsAlias :: AAResultsC t => Ptr t
               -> Ptr MemoryLocation -> Ptr MemoryLocation
               -> IO AliasResult
aaResultsAlias ptr l1 l2 = fmap toAliasResult $ aaResultsAlias_ ptr l1 l2

newMemoryLocation :: ValueC v => Ptr v -> Word64 -> Ptr AAMDNodes
                  -> IO (Ptr MemoryLocation)
newMemoryLocation = newMemoryLocation_

instance Storable MemoryLocation where
  sizeOf _ = fromIntegral memoryLocationSizeOf
  alignment _ = fromIntegral memoryLocationAlignOf
  peek loc = do
    ptr <- memoryLocationGetPtr loc
    size <- memoryLocationGetSize loc
    aatags <- memoryLocationGetAATags loc >>= peek
    return $ MemoryLocation ptr size aatags
  poke loc (MemoryLocation ptr size aatags) = do
    memoryLocationSetPtr loc ptr
    memoryLocationSetSize loc size
    alloca $ \aatags' -> do
      poke aatags' aatags
      memoryLocationSetAATags loc aatags'
#endif
