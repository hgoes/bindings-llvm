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
        newAAMDNodes,
#else
        namedMDNodeGetName,
#endif
#if HS_LLVM_VERSION>=306
        MDTuple(),
        mdTupleGet,
#endif
#if HS_LLVM_VERSION>=307
        MDTupleTypedArrayWrapperC(..),
        MDTupleTypedArrayWrapper(),
        TypedDINodeRefC(..),
        TypedDINodeRef(),
        DINodeC(..),
        DINode(),
        DILocation(),
        newDILocation,
        diLocationGetLine,
        diLocationGetScope,
        diLocationGetInlinedAt,
        DIScopeC(..),
        DIScope(),
        diScopeGetFile,
        diScopeGetName,
        DIFile(),
        newDIFile,
        diFileGetFilename,
        diFileGetDirectory,
        DILocalScopeC(..),
        DILocalScope(),
#if HS_LLVM_VERSION>=308
        diLocalScopeGetSubprogram,
#endif
        DISubprogram(),
        newDISubprogram,
        diSubprogramGetScope,
        diSubprogramGetName,
        diSubprogramGetLinkageName,
        diSubprogramGetFile,
        diSubprogramGetLine,
        diSubprogramGetType,
#if HS_LLVM_VERSION>=308
        diSubprogramGetIsLocalToUnit,
        diSubprogramGetIsDefinition,
#endif
        diSubprogramGetScopeLine,
        diSubprogramGetContainingType,
        diSubprogramGetVirtuality,
        diSubprogramGetVirtualIndex,
        diSubprogramGetFlags,
#if HS_LLVM_VERSION>=308
        diSubprogramGetIsOptimized,
#endif
        diSubprogramGetFunction,
        diSubprogramGetTemplateParams,
        diSubprogramGetDeclaration,
        diSubprogramGetVariables,
        DITypeC(..),
        DIType(),
        diTypeGetLine,
        DISubroutineType(),
        DIVariableC(..),
        DIVariable(),
        diVariableGetLine,
        DILocalVariable(),
        newDILocalVariable,
        diLocalVariableGetScope,
        diLocalVariableGetName,
        diLocalVariableGetLine,
        diLocalVariableGetType,
        diLocalVariableGetArg,
        diLocalVariableGetFlags,
        DITemplateParameterC(..),
        DITemplateParameter(),
        diTemplateParameterGetName,
        diTemplateParameterGetType,
        DITemplateTypeParameter(),
        newDITemplateTypeParameter,
#endif
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.OOP
import LLVM.FFI.Value
import LLVM.FFI.IPList
import LLVM.FFI.CPP
import LLVM.FFI.SmallVector
import LLVM.FFI.ArrayRef
import Foreign
import Foreign.C

#include "Helper.h"

TYPE_LEAF(MDString)
#if HS_LLVM_VERSION>=306
TYPE(Metadata)
SUBTYPE(Metadata,MDString)
TYPE(MDNode)
SUBTYPE(Metadata,MDNode)
TYPE(MDTuple)
SUBTYPE(Metadata,MDTuple)
SPECIALIZE_ARRAYREF(MDTuple)
#if HS_LLVM_VERSION>=307
TYPE(DINode)
SUBTYPE2(Metadata,MDNode,DINode)
TYPE(DIScope)
SUBTYPE3(Metadata,MDNode,DINode,DIScope)
SUBTYPE4(Metadata,MDNode,DINode,DIScope,DIFile)
TYPE(DILocalScope)
SUBTYPE4(Metadata,MDNode,DINode,DIScope,DILocalScope)
SUBTYPE5(Metadata,MDNode,DINode,DIScope,DILocalScope,DISubprogram)
TYPE(DIType)
SUBTYPE4(Metadata,MDNode,DINode,DIScope,DIType)
SUBTYPE5(Metadata,MDNode,DINode,DIScope,DIType,DISubroutineType)
TYPE(DIVariable)
SUBTYPE3(Metadata,MDNode,DINode,DIVariable)
SUBTYPE4(Metadata,MDNode,DINode,DIVariable,DILocalVariable)
TYPE(DITemplateParameter)
SUBTYPE3(Metadata,MDNode,DINode,DITemplateParameter)
SUBTYPE4(Metadata,MDNode,DINode,DITemplateParameter,DITemplateTypeParameter)
#endif
#else
TYPE_LEAF(MDNode)
SUBTYPE(Value,MDNode)
SUBTYPE(Value,MDString)

GETTYPE(MDNode)
GETTYPE(MDString)
#endif

SPECIALIZE_IPLIST(NamedMDNode,capi)

#if HS_LLVM_VERSION>=306
metadataGetID :: MetadataC m => Ptr m -> IO CUInt
metadataGetID = metadataGetID_

metadataDump :: MetadataC m => Ptr m -> IO ()
metadataDump = metadataDump_
#endif

#if HS_LLVM_VERSION>=307
class MDTupleTypedArrayWrapperC obj where
  newMDTupleTypedArrayWrapper :: Ptr MDTuple -> IO (Ptr (MDTupleTypedArrayWrapper obj))

instance MDTupleTypedArrayWrapperC DILocalVariable where
  newMDTupleTypedArrayWrapper = newMDTupleTypedArrayWrapperLocalVariable

diTemplateParameterGetName :: DITemplateParameterC tp
                           => Ptr tp
                           -> IO (Ptr StringRef)
diTemplateParameterGetName = diTemplateParameterGetName_

diTemplateParameterGetType :: DITemplateParameterC tp
                           => Ptr tp
                           -> IO (Ptr (TypedDINodeRef DIType))
diTemplateParameterGetType = diTemplateParameterGetType_

diVariableGetLine :: DIVariableC var
                  => Ptr var
                  -> IO CUInt
diVariableGetLine = diVariableGetLine_

newDILocalVariable :: DILocalScopeC scope
                   => Ptr LLVMContext
#if HS_LLVM_VERSION<308
                   -> CUInt
#endif
                   -> Ptr scope
                   -> Ptr StringRef
                   -> Ptr DIFile
                   -> CUInt
                   -> Ptr (TypedDINodeRef DIType)
                   -> CUInt
                   -> CUInt
                   -> IO (Ptr DILocalVariable)
newDILocalVariable = newDILocalVariable_

newDILocation :: DILocalScopeC diLocalScope
              => Ptr LLVMContext
              -> CUInt -> CUInt
              -> Ptr diLocalScope
              -> Ptr DILocation
              -> IO (Ptr DILocation)
newDILocation = newDILocation_

diScopeGetFile :: DIScopeC diScope => Ptr diScope -> IO (Ptr DIFile)
diScopeGetFile = diScopeGetFile_

diScopeGetName :: DIScopeC diScope => Ptr diScope -> IO (Ptr StringRef)
diScopeGetName = diScopeGetName_

#if HS_LLVM_VERSION>=308
diLocalScopeGetSubprogram :: DILocalScopeC diLocalScope => Ptr diLocalScope -> IO (Ptr DISubprogram)
diLocalScopeGetSubprogram = diLocalScopeGetSubprogram_
#endif

class TypedDINodeRefC obj where
  newTypedDINodeRef :: MetadataC ref => Ptr ref -> IO (Ptr (TypedDINodeRef obj))
  typedDINodeRefGet :: Ptr obj -> IO (Ptr (TypedDINodeRef obj))

instance TypedDINodeRefC DIScope where
  newTypedDINodeRef = newTypedDINodeRefScope
  typedDINodeRefGet = typedDINodeRefGetScope

instance TypedDINodeRefC DIType where
  newTypedDINodeRef = newTypedDINodeRefType
  typedDINodeRefGet = typedDINodeRefGetType
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
