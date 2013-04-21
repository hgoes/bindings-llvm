module LLVM.FFI.Loop
       (LoopBaseC(..)
       ,LoopBase()
       ,Loop()
       ,loopIsLoopInvariant
       ,loopDump
       ,LoopInfoBaseC(..)
       ,LoopInfoBase()
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Value
import LLVM.FFI.CPP

import Foreign.Ptr
import Foreign.C

#include "Helper.h"

SPECIALIZE_VECTOR(Loop)

class LoopBaseC t where
  type LoopBaseBlock t
  type LoopBaseLoop t
  loopGetDepth :: Ptr t -> IO CUInt
  loopGetHeader :: Ptr t -> IO (Ptr (LoopBaseBlock t))
  loopGetParent :: Ptr t -> IO (Ptr (LoopBaseLoop t))
  loopContainsLoop :: Ptr t -> Ptr (LoopBaseLoop t) -> IO Bool
  loopContainsBlock :: Ptr t -> Ptr (LoopBaseBlock t) -> IO Bool
  loopGetSubLoops :: Ptr t -> IO (Ptr (Vector (Ptr (LoopBaseLoop t))))
  loopGetBlocks :: Ptr t -> IO (Ptr (Vector (Ptr (LoopBaseBlock t))))

instance LoopBaseC (LoopBase BasicBlock Loop) where
  type LoopBaseBlock (LoopBase BasicBlock Loop) = BasicBlock
  type LoopBaseLoop (LoopBase BasicBlock Loop) = Loop
  loopGetDepth = loopGetDepth_
  loopGetHeader = loopGetHeader_
  loopGetParent = loopGetParent_
  loopContainsLoop = loopContainsLoop_
  loopContainsBlock = loopContainsBlock_
  loopGetSubLoops = loopGetSubLoops_
  loopGetBlocks = loopGetBlocks_
  
instance LoopBaseC Loop where
  type LoopBaseBlock Loop = BasicBlock
  type LoopBaseLoop Loop = Loop
  loopGetDepth = loopGetDepth_
  loopGetHeader = loopGetHeader_
  loopGetParent = loopGetParent_
  loopContainsLoop = loopContainsLoop_
  loopContainsBlock = loopContainsBlock_
  loopGetSubLoops = loopGetSubLoops_
  loopGetBlocks = loopGetBlocks_

class LoopInfoBaseC blk loop where
  loopInfoBaseBegin :: Ptr (LoopInfoBase blk loop) -> IO (Ptr (Const_iterator (Ptr loop)))
  loopInfoBaseEnd :: Ptr (LoopInfoBase blk loop) -> IO (Ptr (Const_iterator (Ptr loop)))
  loopInfoBaseGetLoopFor :: Ptr (LoopInfoBase blk loop) -> Ptr blk -> IO (Ptr loop)

instance LoopInfoBaseC BasicBlock Loop where
  loopInfoBaseBegin = loopInfoBaseBegin_
  loopInfoBaseEnd = loopInfoBaseEnd_
  loopInfoBaseGetLoopFor = loopInfoBaseGetLoopFor_

loopIsLoopInvariant :: ValueC v => Ptr Loop -> Ptr v -> IO Bool
loopIsLoopInvariant = loopIsLoopInvariant_
