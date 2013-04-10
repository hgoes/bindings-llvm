module LLVM.FFI.AliasAnalysis 
       (AliasAnalysis()
       ,AliasAnalysisC()
       ,newAliasAnalysis
       ,deleteAliasAnalysis
       ,aliasAnalysisGetTargetLibraryInfo
       ,aliasAnalysisGetTypeStoreSize
       ,aliasAnalysisGetLocationLoad
       ,aliasAnalysisGetLocationStore
       ,aliasAnalysisGetLocationVAArg
       ,aliasAnalysisGetLocationAtomicCmpXchg
       ,aliasAnalysisGetLocationAtomicRMWInst
       ,aliasAnalysisAlias
       ,AliasResult(..)
       ,Location()
       ,newLocation
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Value
import LLVM.FFI.Pass (TargetLibraryInfo)
import LLVM.FFI.Type (TypeC)
import LLVM.FFI.Instruction (LoadInst,StoreInst,VAArgInst,AtomicCmpXchgInst,AtomicRMWInst)
import Foreign.C
import Foreign.Ptr
import Data.Word

#include "Helper.h"

class AliasAnalysisC t

deleteAliasAnalysis :: AliasAnalysisC t => Ptr t -> IO ()
deleteAliasAnalysis = deleteAliasAnalysis_

aliasAnalysisGetTargetLibraryInfo :: AliasAnalysisC t => Ptr t -> IO (Ptr TargetLibraryInfo)
aliasAnalysisGetTargetLibraryInfo = aliasAnalysisGetTargetLibraryInfo_

aliasAnalysisGetTypeStoreSize :: (AliasAnalysisC a,TypeC tp) => Ptr a -> Ptr tp -> IO Word64
aliasAnalysisGetTypeStoreSize = aliasAnalysisGetTypeStoreSize_

aliasAnalysisGetLocationLoad :: AliasAnalysisC t => Ptr t -> Ptr LoadInst -> IO (Ptr Location)
aliasAnalysisGetLocationLoad = aliasAnalysisGetLocationLoad_

aliasAnalysisGetLocationStore :: AliasAnalysisC t => Ptr t -> Ptr StoreInst -> IO (Ptr Location)
aliasAnalysisGetLocationStore = aliasAnalysisGetLocationStore_

aliasAnalysisGetLocationVAArg :: AliasAnalysisC t => Ptr t -> Ptr VAArgInst -> IO (Ptr Location)
aliasAnalysisGetLocationVAArg = aliasAnalysisGetLocationVAArg_

aliasAnalysisGetLocationAtomicCmpXchg :: AliasAnalysisC t => Ptr t -> Ptr AtomicCmpXchgInst -> IO (Ptr Location)
aliasAnalysisGetLocationAtomicCmpXchg = aliasAnalysisGetLocationAtomicCmpXchg_

aliasAnalysisGetLocationAtomicRMWInst :: AliasAnalysisC t => Ptr t -> Ptr AtomicRMWInst -> IO (Ptr Location)
aliasAnalysisGetLocationAtomicRMWInst = aliasAnalysisGetLocationAtomicRMW_

aliasAnalysisAlias :: AliasAnalysisC t => Ptr t -> Ptr Location -> Ptr Location -> IO AliasResult
aliasAnalysisAlias ptr l1 l2 = fmap toAliasResult $ aliasAnalysisAlias_ ptr l1 l2

newLocation :: ValueC v => Ptr v -> Word64 -> Ptr MDNode -> IO (Ptr Location)
newLocation ptr size tbaa = newLocation_ ptr size tbaa

data AliasResult =
#define HANDLE_ALIAS_RESULT(name) PRESERVE(  ) name
#define HANDLE_SEP_ALIAS_RESULT PRESERVE(  ) |
#include "Alias.def"
  deriving (Show,Eq,Ord)

#define HANDLE_ALIAS_RESULT(name) foreign import capi _TO_STRING(extra.h AliasResult_##name) aliasResult_##name :: CInt
#include "Alias.def"

toAliasResult :: CInt -> AliasResult
toAliasResult op
#define HANDLE_ALIAS_RESULT(name) PRESERVE (  ) | op == aliasResult_##name = name
#include "Alias.def"