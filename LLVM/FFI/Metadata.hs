module LLVM.FFI.Metadata
#if HS_LLVM_VERSION>=306
       (Metadata(),
        MetadataC(),
        metadataGetID,
        metadataDump,
        MDNode(),
#else        
       (MDNode(),
#endif
        newMDNode,
        mdNodeGetNumOperands,
        mdNodeGetOperand,
#if HS_LLVM_VERSION<306
        mdNodeIsFunctionLocal,
        mdNodeGetFunction,
#endif
        MDString(),
        newMDString,
        mdStringGetString,
        NamedMDNode(),
        namedMDNodeGetParent,
        namedMDNodeGetOperand,
        namedMDNodeGetNumOperands,
        namedMDNodeAddOperand,
#if HS_LLVM_VERSION>305
        namedMDNodeGetName,
        AAMDNodes(..),
        newAAMDNodes
#else
        namedMDNodeGetName
#endif
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.OOP
import LLVM.FFI.Value
import LLVM.FFI.IPList
import LLVM.FFI.CPP
import LLVM.FFI.SmallVector
import Foreign
import Foreign.C

#include "Helper.h"

TYPE_LEAF(MDNode)
TYPE_LEAF(MDString)
#if HS_LLVM_VERSION>=306
SUBTYPE(Metadata,MDNode)
SUBTYPE(Metadata,MDString)
#else
SUBTYPE(Value,MDNode)
SUBTYPE(Value,MDString)

GETTYPE(MDNode)
GETTYPE(MDString)
#endif

SPECIALIZE_IPLIST(NamedMDNode,capi)

#if HS_LLVM_VERSION>=306
class MetadataC m

metadataGetID :: MetadataC m => Ptr m -> IO CUInt
metadataGetID = metadataGetID_

metadataDump :: MetadataC m => Ptr m -> IO ()
metadataDump = metadataDump_
#endif

instance PairC CUInt (Ptr MDNode) where
  pairSize _ = sizeofPairUnsigned_MDNode
  pairFirst = pairFirstUnsigned_MDNode
  pairSecond = pairSecondUnsigned_MDNode
  pairSetFirst = pairSetFirstUnsigned_MDNode
  pairSetSecond = pairSetSecondUnsigned_MDNode

instance Storable (Pair CUInt (Ptr MDNode)) where
  sizeOf _ = fromIntegral $ sizeofPairUnsigned_MDNode
  alignment _ = fromIntegral $ sizeofPairUnsigned_MDNode
  peek ptr = do
    s1 <- pairFirstUnsigned_MDNode ptr
    s2 <- pairSecondUnsigned_MDNode ptr
    return (Pair s1 s2)
  poke ptr (Pair s1 s2) = do
    pairSetFirstUnsigned_MDNode ptr s1
    pairSetSecondUnsigned_MDNode ptr s2

instance SmallVectorC (Pair CUInt (Ptr MDNode)) where
  newSmallVector = newSmallVectorMDNodePair
  deleteSmallVector = deleteSmallVectorMDNodePair
  smallVectorSize = smallVectorSizeMDNodePair
  smallVectorData = smallVectorDataMDNodePair

#if HS_LLVM_VERSION>305
instance Storable AAMDNodes where
  sizeOf _ = fromIntegral aaMDNodesSizeOf
  alignment _ = fromIntegral aaMDNodesAlignOf
  peek ptr = do
    tbaa <- aaMDNodesGetTBAA ptr
    scope <- aaMDNodesGetScope ptr
    noalias <- aaMDNodesGetNoAlias ptr
    return $ AAMDNodes tbaa scope noalias
  poke ptr node = do
    aaMDNodesSetTBAA ptr (aaMDNodesTBAA node)
    aaMDNodesSetScope ptr (aaMDNodesScope node)
    aaMDNodesSetNoAlias ptr (aaMDNodesNoAlias node)
#endif
