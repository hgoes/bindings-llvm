module LLVM.FFI.Loop
       (LoopBaseC(..)
       ,LoopBase()
       ,Loop()
       ,loopIsLoopInvariant
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

loopIsLoopInvariant :: ValueC v => Ptr Loop -> Ptr v -> IO Bool
loopIsLoopInvariant = loopIsLoopInvariant_
