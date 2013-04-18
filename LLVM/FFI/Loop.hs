module LLVM.FFI.Loop
       (LoopBaseC(..)
       ,LoopBase()
       ,Loop()
       ,loopIsLoopInvariant
       ,LoopInfoBaseC(..)
       ,LoopInfoBase()
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Value
import LLVM.FFI.CPP

import Foreign.Ptr
import Foreign.C

class LoopBaseC t blk loop where
  loopGetDepth :: Ptr t -> IO CUInt
  loopGetHeader :: Ptr t -> IO (Ptr blk)
  loopGetParent :: Ptr t -> IO (Ptr loop)
  loopContainsLoop :: Ptr t -> Ptr loop -> IO Bool
  loopContainsBlock :: Ptr t -> Ptr blk -> IO Bool
  loopGetSubLoops :: Ptr t -> IO (Ptr (Vector (Ptr loop)))
  loopGetBlocks :: Ptr t -> IO (Ptr (Vector (Ptr blk)))

instance LoopBaseC (LoopBase BasicBlock Loop) BasicBlock Loop where
  loopGetDepth = loopGetDepth_
  loopGetHeader = loopGetHeader_
  loopGetParent = loopGetParent_
  loopContainsLoop = loopContainsLoop_
  loopContainsBlock = loopContainsBlock_
  loopGetSubLoops = loopGetSubLoops_
  loopGetBlocks = loopGetBlocks_
  
instance LoopBaseC Loop BasicBlock Loop where
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
