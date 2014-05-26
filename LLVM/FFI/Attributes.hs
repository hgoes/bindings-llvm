module LLVM.FFI.Attributes
#if HS_LLVM_VERSION>=303
       (AttributeSet()
       ,newAttributeSet
       ,deleteAttributeSet
       ,attributeSetAddAttribute
       ,AttrKind(..)
#else
       (Attributes()
       ,newAttributes
       ,AttrListPtr()
       ,newAttrListPtr
       ,attrListPtrAddAttr
#if HS_LLVM_VERSION==301
       ,newAttributesFromAttr
       ,attributesUnion
       ,attributesIntersection
#endif
#if HS_LLVM_VERSION>=302
       ,attributesGet
       ,AttrBuilder()
       ,newAttrBuilder
       ,attrBuilderAdd
#endif
       ,deleteAttributes
       ,AttrVal(..)
#endif
       ) where

import LLVM.FFI.Interface
import Data.Typeable
import Data.Bits (shiftL)

#if HS_LLVM_VERSION<=301

import Foreign.Ptr
import Data.Word
    
#include "Helper.h"

data AttrVal = AttrNone
             | AttrZExt
             | AttrSExt
             | AttrNoReturn
             | AttrInReg
             | AttrStructRet
             | AttrNoUnwind
             | AttrNoAlias
             | AttrByVal
             | AttrNest
             | AttrReadNone
             | AttrReadOnly
             | AttrNoInline
             | AttrAlwaysInline
             | AttrOptimizeForSize
             | AttrStackProtect
             | AttrStackProtectReq
             | AttrAlignment
             | AttrNoCapture
             | AttrNoRedZone
             | AttrNoImplicitFloat
             | AttrNaked
             | AttrInlineHint
             | AttrStackAlignment
             | AttrReturnsTwice
             | AttrUWTable
             | AttrNonLazyBind
             | AttrAddressSafety
             deriving (Show,Eq,Ord,Typeable)

#if HS_LLVM_VERSION==301
newAttributesFromAttr :: AttrVal -> IO (Ptr Attributes)
newAttributesFromAttr w = newAttributesFromAttr_ (fromAttrVal w)
#endif

fromAttrVal :: AttrVal -> Word64
#define DECLARE_LLVM_ATTRIBUTE(name, value) fromAttrVal Attr##name = value
DECLARE_LLVM_ATTRIBUTE(None,0)    
DECLARE_LLVM_ATTRIBUTE(ZExt,1 `shiftL` 0) 
DECLARE_LLVM_ATTRIBUTE(SExt,1 `shiftL` 1) 
DECLARE_LLVM_ATTRIBUTE(NoReturn,1 `shiftL` 2) 
DECLARE_LLVM_ATTRIBUTE(InReg,1 `shiftL` 3) 
DECLARE_LLVM_ATTRIBUTE(StructRet,1 `shiftL` 4) 
DECLARE_LLVM_ATTRIBUTE(NoUnwind,1 `shiftL` 5) 
DECLARE_LLVM_ATTRIBUTE(NoAlias,1 `shiftL` 6) 
DECLARE_LLVM_ATTRIBUTE(ByVal,1 `shiftL` 7) 
DECLARE_LLVM_ATTRIBUTE(Nest,1 `shiftL` 8) 
DECLARE_LLVM_ATTRIBUTE(ReadNone,1 `shiftL` 9) 
DECLARE_LLVM_ATTRIBUTE(ReadOnly,1 `shiftL` 10) 
DECLARE_LLVM_ATTRIBUTE(NoInline,1 `shiftL` 11) 
DECLARE_LLVM_ATTRIBUTE(AlwaysInline,1 `shiftL` 12) 
DECLARE_LLVM_ATTRIBUTE(OptimizeForSize,1 `shiftL` 13) 
DECLARE_LLVM_ATTRIBUTE(StackProtect,1 `shiftL` 14) 
DECLARE_LLVM_ATTRIBUTE(StackProtectReq,1 `shiftL` 15) 
DECLARE_LLVM_ATTRIBUTE(Alignment,31 `shiftL` 16) 
DECLARE_LLVM_ATTRIBUTE(NoCapture,1 `shiftL` 21) 
DECLARE_LLVM_ATTRIBUTE(NoRedZone,1 `shiftL` 22) 
DECLARE_LLVM_ATTRIBUTE(NoImplicitFloat,1 `shiftL` 23) 
DECLARE_LLVM_ATTRIBUTE(Naked,1 `shiftL` 24) 
DECLARE_LLVM_ATTRIBUTE(InlineHint,1 `shiftL` 25) 
DECLARE_LLVM_ATTRIBUTE(StackAlignment,7 `shiftL` 26) 
DECLARE_LLVM_ATTRIBUTE(ReturnsTwice,1 `shiftL` 29) 
DECLARE_LLVM_ATTRIBUTE(UWTable,1 `shiftL` 30) 
DECLARE_LLVM_ATTRIBUTE(NonLazyBind,1 `shiftL` 31) 
DECLARE_LLVM_ATTRIBUTE(AddressSafety,1 `shiftL` 32)
#undef DECLARE_LLVM_ATTRIBUTE
#endif
