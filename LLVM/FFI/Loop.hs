module LLVM.FFI.Loop
       (LoopBaseC(..)
       ,LoopBase()
       ,loopExitEdgeList
       ,Loop()
       ,loopIsLoopInvariant
       ,LoopInfoBaseC(..)
       ,LoopInfoBase()
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Value
import LLVM.FFI.SmallVector
import LLVM.FFI.CPP
import LLVM.FFI.BasicBlock

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array

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
  loopGetExitEdges :: Ptr t -> Ptr (SmallVector (Pair (Ptr BasicBlock) (Ptr BasicBlock))) -> IO ()
  loopGetNumBackEdges :: Ptr t -> IO CUInt
  loopGetExitBlocks :: Ptr t -> Ptr (SmallVector (Ptr (LoopBaseBlock t))) -> IO ()
  loopGetExitBlock :: Ptr t -> IO (Ptr (LoopBaseBlock t))

loopExitEdgeList :: LoopBaseC t => Ptr t -> IO [(Ptr BasicBlock,Ptr BasicBlock)]
loopExitEdgeList loop = do
  vec <- newSmallVector
  loopGetExitEdges loop vec
  sz <- smallVectorSize vec
  dat <- smallVectorData vec
  res <- mapM (peekPair undefined dat . fromIntegral) [0..(sz-1)]
  deleteSmallVector vec
  return res
  where
    peekPair :: PairC a b => Pair a b -> Ptr (Pair a b) -> Int -> IO (a,b)
    peekPair u ptr off = pairToTuple (ptr `plusPtr` (off*(fromIntegral $ pairSize u)))

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
  loopGetExitEdges = loopGetExitEdges_
  loopGetNumBackEdges = loopGetNumBackEdges_
  loopGetExitBlocks = loopGetExitBlocks_
  loopGetExitBlock = loopGetExitBlock_
  
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
  loopGetExitEdges = loopGetExitEdges_
  loopGetNumBackEdges = loopGetNumBackEdges_
  loopGetExitBlocks = loopGetExitBlocks_
  loopGetExitBlock = loopGetExitBlock_
  
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

SPECIALIZE_SMALLVECTOR(Loop)
