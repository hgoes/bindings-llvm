module LLVM.FFI.AliasAnalysis 
       (AliasAnalysis()
       ,AliasAnalysisC()
       ,newAliasAnalysis
       ,deleteAliasAnalysis
#if HS_LLVM_VERSION >= 303
       ,aliasAnalysisGetTargetLibraryInfo
#endif
       ,aliasAnalysisGetTypeStoreSize
       ,aliasAnalysisGetLocationLoad
       ,aliasAnalysisGetLocationStore
       ,aliasAnalysisGetLocationVAArg
#if HS_LLVM_VERSION >= 300
       ,aliasAnalysisGetLocationAtomicCmpXchg
       ,aliasAnalysisGetLocationAtomicRMWInst
#endif
       ,aliasAnalysisAlias
       ,AliasResult(..)
       ,Location()
       ,newLocation
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Value
import LLVM.FFI.Pass (TargetLibraryInfo)
import LLVM.FFI.Type (TypeC)
#if HS_LLVM_VERSION >= 300
import LLVM.FFI.Instruction (LoadInst,StoreInst,VAArgInst,AtomicCmpXchgInst,AtomicRMWInst)
#else
import LLVM.FFI.Instruction (LoadInst,StoreInst,VAArgInst)
#endif
import Foreign.C
import Foreign.Ptr
import Data.Word

#include "Helper.h"

class AliasAnalysisC t

deleteAliasAnalysis :: AliasAnalysisC t => Ptr t -> IO ()
deleteAliasAnalysis = deleteAliasAnalysis_

#if HS_LLVM_VERSION >= 303
aliasAnalysisGetTargetLibraryInfo :: AliasAnalysisC t => Ptr t -> IO (Ptr TargetLibraryInfo)
aliasAnalysisGetTargetLibraryInfo = aliasAnalysisGetTargetLibraryInfo_
#endif

aliasAnalysisGetTypeStoreSize :: (AliasAnalysisC a,TypeC tp) => Ptr a -> Ptr tp -> IO Word64
aliasAnalysisGetTypeStoreSize = aliasAnalysisGetTypeStoreSize_

aliasAnalysisGetLocationLoad :: AliasAnalysisC t => Ptr t -> Ptr LoadInst -> IO (Ptr Location)
aliasAnalysisGetLocationLoad = aliasAnalysisGetLocationLoad_

aliasAnalysisGetLocationStore :: AliasAnalysisC t => Ptr t -> Ptr StoreInst -> IO (Ptr Location)
aliasAnalysisGetLocationStore = aliasAnalysisGetLocationStore_

aliasAnalysisGetLocationVAArg :: AliasAnalysisC t => Ptr t -> Ptr VAArgInst -> IO (Ptr Location)
aliasAnalysisGetLocationVAArg = aliasAnalysisGetLocationVAArg_

#if HS_LLVM_VERSION >= 300
aliasAnalysisGetLocationAtomicCmpXchg :: AliasAnalysisC t => Ptr t -> Ptr AtomicCmpXchgInst -> IO (Ptr Location)
aliasAnalysisGetLocationAtomicCmpXchg = aliasAnalysisGetLocationAtomicCmpXchg_

aliasAnalysisGetLocationAtomicRMWInst :: AliasAnalysisC t => Ptr t -> Ptr AtomicRMWInst -> IO (Ptr Location)
aliasAnalysisGetLocationAtomicRMWInst = aliasAnalysisGetLocationAtomicRMW_
#endif

aliasAnalysisAlias :: AliasAnalysisC t => Ptr t -> Ptr Location -> Ptr Location -> IO AliasResult
aliasAnalysisAlias ptr l1 l2 = fmap toAliasResult $ aliasAnalysisAlias_ ptr l1 l2

newLocation :: ValueC v => Ptr v -> Word64 -> Ptr MDNode -> IO (Ptr Location)
newLocation ptr size tbaa = newLocation_ ptr size tbaa
