module LLVM.FFI.BasicBlock
       (BasicBlock()
       ,createBasicBlock
       ,deleteBasicBlock
       ,basicBlockGetParent
       ,getInstList
       ,getTerminator
       )where

import LLVM.FFI.Interface
import LLVM.FFI.OOP
import LLVM.FFI.Value
import LLVM.FFI.Constant
import LLVM.FFI.Instruction
import LLVM.FFI.IPList
import LLVM.FFI.CPP
import LLVM.FFI.SmallVector

import Foreign
import Foreign.C

#include "Helper.h"

TYPE_LEAF(BasicBlock)
SUBTYPE(Value,BasicBlock)

SPECIALIZE_IPLIST(BasicBlock,capi)
SPECIALIZE_VECTOR(BasicBlock)
SPECIALIZE_PAIR(BasicBlock,BasicBlock)

instance SmallVectorC (Pair (Ptr BasicBlock) (Ptr BasicBlock)) where
  newSmallVector = newSmallVectorEdge
  deleteSmallVector = deleteSmallVectorEdge
  smallVectorSize = smallVectorSizeEdge
  smallVectorData = smallVectorDataEdge