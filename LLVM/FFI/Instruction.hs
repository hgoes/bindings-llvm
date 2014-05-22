module LLVM.FFI.Instruction 
       (-- * Instructions
         Instruction(),
         InstructionC(),
         OpType(..),
         toOpCode,
         TermOpType(..),
         toTermOpCode,
         BinOpType(..),
         toBinOpCode,
         MemoryOpType(..),
         toMemoryOpCode,
         CastOpType(..),
         toCastOpCode,
         OtherOpType(..),
         toOtherOpCode,
         CallingConv(..),
         toCallingConv,
#if HS_LLVM_VERSION>=300
         AtomicOrdering(..),
         RMWBinOp(..),
         SynchronizationScope(..),
#endif
         instructionGetParent,
         instructionGetMetadataById,
         instructionGetMetadataByName,
         instructionGetAllMetadata,
         instructionGetDebugLoc,
         instructionIsUsedOutsideOfBlock,
         instructionGetOpcode,
         instructionGetOpcodeName,
#if HS_LLVM_VERSION>=300
         -- ** Atomic Compare & Exchange Instruction
         AtomicCmpXchgInst(),
         newAtomicCmpXchgInst,
         atomicCmpXchgInstIsVolatile,
         atomicCmpXchgInstGetOrdering,
         atomicCmpXchgInstGetPointerOperand,
         atomicCmpXchgInstGetCompareOperand,
         atomicCmpXchgInstGetNewValOperand,
         -- ** Atomic Read & Modify & Write Instruction
         AtomicRMWInst(),
         newAtomicRMWInst,
         atomicRMWInstGetOperation,
         atomicRMWInstIsVolatile,
         atomicRMWInstGetOrdering,
         atomicRMWInstGetPointerOperand,
         atomicRMWInstGetValOperand,
#endif
         -- ** Binary Operator
         BinaryOperator(),
         newBinaryOperator,
         binOpGetOpCode,
         binOpHasNoUnsignedWrap,
         binOpHasNoSignedWrap,
         binOpIsExact,
         -- ** Call Instructions
         CallInst(),
         newCallInst,
         callInstIsTailCall,
         callInstGetNumArgOperands,
         callInstGetArgOperand,
         callInstGetCallingConv,
         callInstGetCalledValue,
         isMallocLikeFn,
         getMallocAllocatedType,
         getMallocArraySize,
         -- ** Compare Instructions
         CmpInst(),
         CmpInstC(),
         Predicate(..),
         cmpInstGetPredicate,
         -- *** Float Compare Instruction
         FCmpInst(),
         FCmpOp(..),
         newFCmpInst,
         getFCmpOp,
         inverseFCmpOp,
         swappedFCmpOp,
         -- *** Integer Compare Instruction
         ICmpInst(),
         ICmpOp(..),
         newICmpInst,
         getICmpOp,
         inverseICmpOp,
         swappedICmpOp,
         -- ** Extract Element Instruction
         ExtractElementInst(),
         newExtractElementInst,
         extractElementInstGetVectorOperand,
         extractElementInstGetIndexOperand,
#if HS_LLVM_VERSION>=300
         -- ** Fence Instruction
         FenceInst(),
         newFenceInst,
         fenceInstGetOrdering,
#endif
         -- ** Get Element Pointer Instruction
         GetElementPtrInst(),
         newGetElementPtrInst,
         getElementPtrInstGetPointerOperand,
         getElementPtrInstIdxBegin,
         getElementPtrInstIdxEnd,
         getElementPtrInstIsInBounds,
         getElementPtrInstGetNumIndices,
         -- ** Insert Element Instruction
         InsertElementInst(),
         newInsertElementInst,
         -- ** Insert Value Instruction
         InsertValueInst(),
         newInsertValueInst,
#if HS_LLVM_VERSION>=300
         -- ** Landing Pad Instruction
         LandingPadInst(),
         newLandingPadInst,
         landingPadInstGetPersonaliteFn,
         landingPadInstIsCleanup,
         landingPadInstSetCleanup,
         landingPadInstGetNumClauses,
         landingPadInstGetClause,
         landingPadInstAddClause,
         landingPadInstIsCatch,
         landingPadInstIsFilter,
#endif
         -- ** PHI-Node
         PHINode(),
         newPhiNode,
         phiNodeGetNumIncomingValues,
         phiNodeGetIncomingValue,
         phiNodeGetIncomingBlock,
         phiNodeAddIncoming,
         -- ** Selection Instruction
         SelectInst(),
         newSelectInst,
         selectInstGetCondition,
         selectInstGetTrueValue,
         selectInstGetFalseValue,
         -- ** Shuffle Vector Instruction
         ShuffleVectorInst(),
         newShuffleVectorInst,
         -- ** Store Instruction
         StoreInst(),
         newStoreInst,
         storeInstIsVolatile,
         storeInstGetAlignment,
#if HS_LLVM_VERSION>=300
         storeInstGetOrdering,
#endif
         storeInstGetValueOperand,
         storeInstGetPointerOperand,
         -- ** Terminator Instructions
         TerminatorInst(),
         TerminatorInstC(),
         terminatorInstGetNumSuccessors,
         terminatorInstGetSuccessor,
         -- *** Branch Instruction
         BranchInst(),
         newBranchInst,
         newBranchInstCond,
         branchInstIsConditional,
         branchInstGetCondition,
         -- *** Indirect Branch Instruction
         IndirectBrInst(),
         newIndirectBrInst,
         indirectBrInstGetAddress,
         indirectBrInstGetNumDestinations,
         indirectBrInstGetDestination,
         indirectBrInstAddDestination,
         -- *** Invoke Instruction
         InvokeInst(),
         newInvokeInst,
         invokeInstGetNumArgOperands,
         invokeInstGetArgOperand,
         invokeInstGetCallingConv,
         invokeInstGetCalledValue,
         invokeInstGetNormalDest,
         invokeInstGetUnwindDest,
#if HS_LLVM_VERSION>=300
         invokeInstGetLandingPadInst,
         -- *** Resume Instruction
         ResumeInst(),
         newResumeInst,
         resumeInstGetValue,
#endif
         -- *** Return Instruction
         ReturnInst(),
         newReturnInst,
         returnInstGetReturnValue,
         -- *** Switch Instruction
         SwitchInst(),
         newSwitchInst,
         switchInstGetCondition,
         switchInstGetDefaultDest,
#if HS_LLVM_VERSION>=301
         CaseIt(),
         switchInstCaseBegin,
         switchInstCaseEnd,
         switchInstCaseDefault,
         caseItNext,
         caseItPrev,
         caseItEq,
         caseItGetCaseValue,
         caseItGetCaseSuccessor,
#else
         switchInstGetNumCases,
         switchInstGetCaseValue,
#endif
         -- *** Unreachable Instruction
         UnreachableInst(),
         newUnreachableInst,
         -- ** Unary Instructions
         UnaryInstruction(),
         UnaryInstructionC(),
         -- *** Allocation Instruction
         AllocaInst(),
         newAllocaInst,
         allocaInstIsArrayAllocation,
         allocaInstGetArraySize,
         allocaInstGetAlignment,
         -- *** Casting Instructions
         CastInst(),
         CastInstC(),
         castInstGetOpcode,
         -- **** Bitcasting Instruction
         BitCastInst(),
         newBitCastInst,
#if HS_LLVM_VERSION>=304
         AddrSpaceCastInst(),
         newAddrSpaceCastInst,
#endif
         -- **** Floating Point Extend Instruction
         FPExtInst(),
         newFPExtInst,
         -- **** Floating Point to Signed Integer Instruction
         FPToSIInst(),
         newFPToSIInst,
         -- **** Floating Point to Unsigned Integer Instruction
         FPToUIInst(),
         newFPToUIInst,
         -- **** Floatting Point Truncation Instruction
         FPTruncInst(),
         newFPTruncInst,
         -- **** Integer to Pointer Instruction
         IntToPtrInst(),
         newIntToPtrInst,
         -- **** Pointer to Integer Instruction
         PtrToIntInst(),
         newPtrToIntInst,
         -- **** Signed Extend Instruction
         SExtInst(),
         newSExtInst,
         -- **** Signed Integer to Floating Point Instruction
         SIToFPInst(),
         newSIToFPInst,
         -- **** Truncation Instruction
         TruncInst(),
         newTruncInst,
         -- **** Unsigned Integer to Floating Point Instruction
         UIToFPInst(),
         newUIToFPInst,
         -- **** Zero Extend Instrruction
         ZExtInst(),
         newZExtInst,
         -- *** ExtractValue Instruction
         ExtractValueInst(),
         newExtractValueInst,
         extractValueInstIdxBegin,
         extractValueInstIdxEnd,
         extractValueInstGetNumIndices,
#if HS_LLVM_VERSION>=300
         extractValueInstGetIndices,
#endif
         -- *** Loading Instruction
         LoadInst(),
         newLoadInst,
         loadInstIsVolatile,
         loadInstGetAlignment,
#if HS_LLVM_VERSION>=300
         loadInstGetOrdering,
#endif
         loadInstGetPointerOperand,
         -- *** VarArg Instruction
         VAArgInst(),
         newVAArgInst
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.OOP
import LLVM.FFI.IPList
import LLVM.FFI.Value
import LLVM.FFI.User
import LLVM.FFI.Type

import Foreign
import Foreign.C

#include "Helper.h"

SPECIALIZE_IPLIST(Instruction,capi)

instructionGetParent :: InstructionC i => Ptr i -> IO (Ptr BasicBlock)
instructionGetParent = instructionGetParent_

instructionGetDebugLoc :: InstructionC i => Ptr i -> IO (Ptr DebugLoc)
instructionGetDebugLoc = instructionGetDebugLoc_

instructionGetMetadataById :: InstructionC i => Ptr i -> CUInt -> IO (Ptr MDNode)
instructionGetMetadataById = instructionGetMetadataById_

#if HS_LLVM_VERSION>=301
instructionGetMetadataByName :: InstructionC i => Ptr i -> Ptr StringRef -> IO (Ptr MDNode)
#else
instructionGetMetadataByName :: InstructionC i => Ptr i -> CString -> IO (Ptr MDNode)
#endif
instructionGetMetadataByName = instructionGetMetadataByName_

instructionGetAllMetadata :: InstructionC i => Ptr i -> Ptr (SmallVector (Pair CUInt (Ptr MDNode))) -> IO ()
instructionGetAllMetadata = instructionGetAllMetadata_

instructionIsUsedOutsideOfBlock :: InstructionC i => Ptr i -> Ptr BasicBlock -> IO Bool
instructionIsUsedOutsideOfBlock = instructionIsUsedOutsideOfBlock_

instructionGetOpcode :: InstructionC i => Ptr i -> IO OpType
instructionGetOpcode i = do
  c <- instructionGetOpcode_ i
  let Just code = toOpCode c
  return code

instructionGetOpcodeName :: InstructionC i => Ptr i -> IO String
instructionGetOpcodeName i = do
  strPtr <- instructionGetOpcodeName_ i
  peekCString strPtr

newSelectInst :: (ValueC c,ValueC s1,ValueC s2) => Ptr c -> Ptr s1 -> Ptr s2 -> Ptr Twine -> IO (Ptr SelectInst)
newSelectInst = newSelectInst_

newShuffleVectorInst :: (ValueC v1,ValueC v2,ValueC mask) => Ptr v1 -> Ptr v2 -> Ptr mask -> Ptr Twine -> IO (Ptr ShuffleVectorInst)
newShuffleVectorInst = newShuffleVectorInst_

phiNodeAddIncoming :: ValueC val => Ptr PHINode -> Ptr val -> Ptr BasicBlock -> IO ()
phiNodeAddIncoming = phiNodeAddIncoming_

#if HS_LLVM_VERSION>=300
newPhiNode :: TypeC tp => Ptr tp -> CUInt -> Ptr Twine -> IO (Ptr PHINode)
#else
newPhiNode :: TypeC tp => Ptr tp -> Ptr Twine -> IO (Ptr PHINode)
#endif
newPhiNode = newPhiNode_

#if HS_LLVM_VERSION>=300
landingPadInstAddClause :: ValueC clause => Ptr LandingPadInst -> Ptr clause -> IO ()
landingPadInstAddClause = landingPadInstAddClause_

newLandingPadInst :: (TypeC tp,ValueC fn) => Ptr tp -> Ptr fn -> CUInt -> Ptr Twine -> IO (Ptr LandingPadInst)
newLandingPadInst = newLandingPadInst_
#endif

#if HS_LLVM_VERSION>=300
newInsertValueInst :: (ValueC agg,ValueC val) => Ptr agg -> Ptr val -> Ptr (ArrayRef CUInt) -> Ptr Twine -> IO (Ptr InsertValueInst)
#else
newInsertValueInst :: (ValueC agg,ValueC val) => Ptr agg -> Ptr val -> CUInt -> Ptr Twine -> IO (Ptr InsertValueInst)
#endif
newInsertValueInst = newInsertValueInst_

newInsertElementInst :: (ValueC vec,ValueC newEl,ValueC idx) => Ptr vec -> Ptr newEl -> Ptr idx -> Ptr Twine -> IO (Ptr InsertElementInst)
newInsertElementInst = newInsertElementInst_

#if HS_LLVM_VERSION>=300
newGetElementPtrInst :: ValueC ptr => Ptr ptr -> Ptr (ArrayRef (Ptr Value)) -> Ptr Twine -> IO (Ptr GetElementPtrInst)
#else
newGetElementPtrInst :: ValueC ptr => Ptr ptr -> Ptr Value -> Ptr Twine -> IO (Ptr GetElementPtrInst)
#endif
newGetElementPtrInst = newGetElementPtrInst_

#if HS_LLVM_VERSION>=300
newFenceInst :: Ptr LLVMContext -> AtomicOrdering -> SynchronizationScope -> IO (Ptr FenceInst)
newFenceInst ctx ord sync = newFenceInst_ ctx (fromAtomicOrdering ord) (fromSynchronizationScope sync)
#endif

newExtractElementInst :: (ValueC vec,ValueC idx) => Ptr vec -> Ptr idx -> Ptr Twine -> IO (Ptr ExtractElementInst)
newExtractElementInst = newExtractElementInst_

cmpInstGetPredicate :: Ptr CmpInst -> IO Predicate
cmpInstGetPredicate = fmap toPredicate . cmpInstGetPredicate_

fromICmpOp :: ICmpOp -> CInt
fromICmpOp = fromPredicate . ICmpOp

fromFCmpOp :: FCmpOp -> CInt
fromFCmpOp = fromPredicate . FCmpOp

toICmpOp :: CInt -> ICmpOp
toICmpOp op = op'
  where
    ICmpOp op' = toPredicate op

toFCmpOp :: CInt -> FCmpOp
toFCmpOp op = op'
  where
    FCmpOp op' = toPredicate op

newICmpInst :: (ValueC v1,ValueC v2) => ICmpOp -> Ptr v1 -> Ptr v2 -> Ptr Twine -> IO (Ptr ICmpInst)
newICmpInst op = newICmpInst_ (fromICmpOp op)

newFCmpInst :: (ValueC v1,ValueC v2) => FCmpOp -> Ptr v1 -> Ptr v2 -> Ptr Twine -> IO (Ptr FCmpInst)
newFCmpInst op = newFCmpInst_ (fromFCmpOp op)

#if HS_LLVM_VERSION >= 300
newCallInst :: ValueC fun => Ptr fun -> Ptr (ArrayRef (Ptr Value)) -> Ptr Twine -> IO (Ptr CallInst)
#else
newCallInst :: ValueC fun => Ptr fun -> Ptr Value -> Ptr Twine -> IO (Ptr CallInst)
#endif
newCallInst = newCallInst_

newBinaryOperator :: (ValueC v1,ValueC v2) => BinOpType -> Ptr v1 -> Ptr v2 -> Ptr Twine -> IO (Ptr BinaryOperator)
newBinaryOperator op = newBinaryOperator_ (fromBinOpCode op)

#if HS_LLVM_VERSION >= 300
newAtomicRMWInst :: (ValueC ptr,ValueC value)
                    => RMWBinOp -> Ptr ptr -> Ptr value
                    -> AtomicOrdering -> SynchronizationScope
                    -> IO (Ptr AtomicRMWInst)
newAtomicRMWInst op ptr val ord sync
  = newAtomicRMWInst_ (fromRMWBinOp op) ptr val
    (fromAtomicOrdering ord) (fromSynchronizationScope sync)

newAtomicCmpXchgInst :: (ValueC ptr,ValueC cmp,ValueC newVal)
                        => Ptr ptr -> Ptr cmp -> Ptr newVal
                        -> AtomicOrdering -> SynchronizationScope
                        -> IO (Ptr AtomicCmpXchgInst)
newAtomicCmpXchgInst ptr cmp newVal ord sync
  = newAtomicCmpXchgInst_ ptr cmp newVal (fromAtomicOrdering ord) (fromSynchronizationScope sync)
#endif

#if HS_LLVM_VERSION >= 302
isMallocLikeFn :: ValueC t => Ptr t -> Ptr TargetLibraryInfo -> Bool -> IO Bool
#else
isMallocLikeFn :: ValueC t => Ptr t -> IO Bool
#endif
isMallocLikeFn = isMallocLikeFn_

#if HS_LLVM_VERSION >= 300
newStoreInst :: (ValueC val,ValueC ptr) => Ptr val -> Ptr ptr -> Bool -> CUInt -> AtomicOrdering -> SynchronizationScope -> IO (Ptr StoreInst)
newStoreInst val ptr volatile align aord sync
  = newStoreInst_ val ptr volatile align (fromAtomicOrdering aord) (fromSynchronizationScope sync)
#else
newStoreInst :: (ValueC val,ValueC ptr) => Ptr val -> Ptr ptr -> Bool -> CUInt -> IO (Ptr StoreInst)
newStoreInst = newStoreInst_
#endif

#if HS_LLVM_VERSION >= 300
loadInstGetOrdering :: Ptr LoadInst -> IO AtomicOrdering
loadInstGetOrdering = fmap toAtomicOrdering . loadInstGetOrdering_

storeInstGetOrdering :: Ptr StoreInst -> IO AtomicOrdering
storeInstGetOrdering = fmap toAtomicOrdering . storeInstGetOrdering_

atomicRMWInstGetOperation :: Ptr AtomicRMWInst -> IO RMWBinOp
atomicRMWInstGetOperation = fmap toRMWBinOp . atomicRMWInstGetOperation_

atomicRMWInstGetOrdering :: Ptr AtomicRMWInst -> IO AtomicOrdering
atomicRMWInstGetOrdering = fmap toAtomicOrdering . atomicRMWInstGetOrdering_

atomicCmpXchgInstGetOrdering :: Ptr AtomicCmpXchgInst -> IO AtomicOrdering
atomicCmpXchgInstGetOrdering = fmap toAtomicOrdering . atomicCmpXchgInstGetOrdering_

fenceInstGetOrdering :: Ptr FenceInst -> IO AtomicOrdering
fenceInstGetOrdering = fmap toAtomicOrdering . fenceInstGetOrdering_

landingPadInstIsCatch :: Ptr LandingPadInst -> Integer -> IO Bool
landingPadInstIsCatch ptr i = landingPadInstIsCatch_ ptr (fromInteger i)

landingPadInstIsFilter :: Ptr LandingPadInst -> Integer -> IO Bool
landingPadInstIsFilter ptr i = landingPadInstIsCatch_ ptr (fromInteger i)

landingPadInstGetClause :: Ptr LandingPadInst -> Integer -> IO (Ptr Value)
landingPadInstGetClause ptr i = landingPadInstGetClause_ ptr (fromInteger i)

landingPadInstGetNumClauses :: Ptr LandingPadInst -> IO Integer
landingPadInstGetNumClauses = fmap toInteger . landingPadInstGetNumClauses_
#endif

callInstGetCallingConv :: Ptr CallInst -> IO CallingConv
callInstGetCallingConv = fmap toCallingConv . callInstGetCallingConv_

invokeInstGetCallingConv :: Ptr InvokeInst -> IO CallingConv
invokeInstGetCallingConv = fmap toCallingConv . invokeInstGetCallingConv_

invokeInstGetNumArgOperands :: Ptr InvokeInst -> IO Integer
invokeInstGetNumArgOperands = fmap toInteger . invokeInstGetNumArgOperands_

invokeInstGetArgOperand :: Ptr InvokeInst -> Integer -> IO (Ptr Value)
invokeInstGetArgOperand ptr i = invokeInstGetArgOperand_ ptr (fromInteger i)

#if HS_LLVM_VERSION>=300
newInvokeInst :: ValueC func => Ptr func -> Ptr BasicBlock -> Ptr BasicBlock -> Ptr (ArrayRef (Ptr Value)) -> Ptr Twine -> IO (Ptr InvokeInst)
#else
newInvokeInst :: ValueC func => Ptr func -> Ptr BasicBlock -> Ptr BasicBlock
              -> Ptr (Const_iterator (Ptr Value))
              -> Ptr (Const_iterator (Ptr Value))
              -> Ptr Twine -> IO (Ptr InvokeInst)
#endif
newInvokeInst = newInvokeInst_

#if HS_LLVM_VERSION>=300
newResumeInst :: ValueC val => Ptr val -> IO (Ptr ResumeInst)
newResumeInst = newResumeInst_
#endif

newReturnInst :: ValueC val => Ptr LLVMContext -> Ptr val -> IO (Ptr ReturnInst)
newReturnInst = newReturnInst_

newSwitchInst :: ValueC cond => Ptr cond -> Ptr BasicBlock -> CUInt -> IO (Ptr SwitchInst)
newSwitchInst = newSwitchInst_

getElementPtrInstGetNumIndices :: Ptr GetElementPtrInst -> IO Integer
getElementPtrInstGetNumIndices ptr = fmap toInteger (getElementPtrInstGetNumIndices_ ptr)

loadInstGetAlignment :: Ptr LoadInst -> IO Integer
loadInstGetAlignment ptr = fmap toInteger (loadInstGetAlignment_ ptr)

newAllocaInst :: (TypeC tp,ValueC arrsz) => Ptr tp -> Ptr arrsz -> CUInt -> Ptr Twine -> IO (Ptr AllocaInst)
newAllocaInst = newAllocaInst_

allocaInstGetAlignment :: Ptr AllocaInst -> IO Integer
allocaInstGetAlignment ptr = fmap toInteger (allocaInstGetAlignment_ ptr)

storeInstGetAlignment :: Ptr StoreInst -> IO Integer
storeInstGetAlignment ptr = fmap toInteger (storeInstGetAlignment_ ptr)

terminatorInstGetNumSuccessors :: TerminatorInstC t => Ptr t -> IO Integer
terminatorInstGetNumSuccessors ptr = fmap toInteger $ terminatorInstGetNumSuccessors_ ptr

terminatorInstGetSuccessor :: TerminatorInstC t => Ptr t -> Integer -> IO (Ptr BasicBlock)
terminatorInstGetSuccessor ptr idx = terminatorInstGetSuccessor_ ptr (fromInteger idx)

phiNodeGetNumIncomingValues :: Ptr PHINode -> IO Integer
phiNodeGetNumIncomingValues ptr = fmap toInteger (phiNodeGetNumIncomingValues_ ptr)

phiNodeGetIncomingValue :: Ptr PHINode -> Integer -> IO (Ptr Value)
phiNodeGetIncomingValue ptr idx = phiNodeGetIncomingValue_ ptr (fromInteger idx)

phiNodeGetIncomingBlock :: Ptr PHINode -> Integer -> IO (Ptr BasicBlock)
phiNodeGetIncomingBlock ptr idx = phiNodeGetIncomingBlock_ ptr (fromInteger idx)

newBranchInstCond :: ValueC cond => Ptr BasicBlock -> Ptr BasicBlock -> Ptr cond -> IO (Ptr BranchInst)
newBranchInstCond = newBranchInstCond_

newIndirectBrInst :: ValueC address => Ptr address -> CUInt -> IO (Ptr IndirectBrInst)
newIndirectBrInst = newIndirectBrInst_

castInstGetOpcode :: CastInstC cast => Ptr cast -> IO CastOpType
castInstGetOpcode i = do
  c <- castInstGetOpcode_ i
  let Just code = toCastOpCode c
  return code

newBitCastInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr BitCastInst)
newBitCastInst = newBitCastInst_

#if HS_LLVM_VERSION>=304
newAddrSpaceCastInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr AddrSpaceCastInst)
newAddrSpaceCastInst = newAddrSpaceCastInst_
#endif

newFPExtInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr FPExtInst)
newFPExtInst = newFPExtInstInst_

newFPToSIInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr FPToSIInst)
newFPToSIInst = newFPToSIInst_

newFPToUIInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr FPToUIInst)
newFPToUIInst = newFPToUIInst_

newFPTruncInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr FPTruncInst)
newFPTruncInst = newFPTruncInst_

newIntToPtrInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr IntToPtrInst)
newIntToPtrInst = newIntToPtrInst_

newPtrToIntInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr PtrToIntInst)
newPtrToIntInst = newPtrToIntInst_

newSExtInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr SExtInst)
newSExtInst = newSExtInst_

newSIToFPInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr SIToFPInst)
newSIToFPInst = newSIToFPInst_

newTruncInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr TruncInst)
newTruncInst = newTruncInst_

newUIToFPInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr UIToFPInst)
newUIToFPInst = newUIToFPInst_

newZExtInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr ZExtInst)
newZExtInst = newZExtInst_

#if HS_LLVM_VERSION>=300
newExtractValueInst :: ValueC val => Ptr val -> Ptr (ArrayRef CUInt) -> Ptr Twine
                    -> IO (Ptr ExtractValueInst)
#else
newExtractValueInst :: ValueC val => Ptr val
                    -> Ptr (Const_iterator CUInt)
                    -> Ptr (Const_iterator CUInt)
                    -> Ptr Twine
                    -> IO (Ptr ExtractValueInst)
#endif
newExtractValueInst = newExtractValueInst_

#if HS_LLVM_VERSION>=300
newLoadInst :: ValueC val => Ptr val -> Ptr Twine -> Bool -> CUInt -> AtomicOrdering -> SynchronizationScope -> IO (Ptr LoadInst)
newLoadInst val name volatile align ordering scope
  = newLoadInst_ val name volatile align (fromAtomicOrdering ordering) (fromSynchronizationScope scope)
#else
newLoadInst :: ValueC val => Ptr val -> Ptr Twine -> Bool -> CUInt -> IO (Ptr LoadInst)
newLoadInst = newLoadInst_
#endif

newVAArgInst :: (ValueC val,TypeC tp) => Ptr val -> Ptr tp -> Ptr Twine -> IO (Ptr VAArgInst)
newVAArgInst = newVAArgInst_

data OpType 
  = TermOp TermOpType
  | BinOp BinOpType
  | MemoryOp MemoryOpType
  | CastOp CastOpType
  | OtherOp OtherOpType
  deriving (Show,Eq,Ord)

data TermOpType =
#define HANDLE_TERM_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
  UnknownTermOp
  deriving (Show,Eq,Ord)

data BinOpType =
#define HANDLE_BINARY_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
  UnknownBinOp
  deriving (Show,Eq,Ord)

data MemoryOpType =
#define HANDLE_MEMORY_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
  UnknownMemoryOp
  deriving (Show,Eq,Ord)

data CastOpType =
#define HANDLE_CAST_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
  UnknownCastOp
  deriving (Show,Eq,Ord)

data OtherOpType =
#define HANDLE_OTHER_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
  UnknownOtherOp
  deriving (Show,Eq,Ord)

toTermOpCode :: Integral a => a -> Maybe TermOpType
#define HANDLE_TERM_INST(N,OPC,CLASS) toTermOpCode N = Just OPC
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
toTermOpCode _ = Nothing

toBinOpCode :: Integral a => a -> Maybe BinOpType
#define HANDLE_BINARY_INST(N,OPC,CLASS) toBinOpCode N = Just OPC
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
toBinOpCode _ = Nothing

fromBinOpCode :: BinOpType -> CInt
#define HANDLE_BINARY_INST(N,OPC,CLASS) fromBinOpCode OPC = N
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif

toMemoryOpCode :: Integral a => a -> Maybe MemoryOpType
#define HANDLE_MEMORY_INST(N,OPC,CLASS) toMemoryOpCode N = Just OPC
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
toMemoryOpCode _ = Nothing

toCastOpCode :: Integral a => a -> Maybe CastOpType
#define HANDLE_CAST_INST(N,OPC,CLASS) toCastOpCode N = Just OPC
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
toCastOpCode _ = Nothing

toOtherOpCode :: Integral a => a -> Maybe OtherOpType
#define HANDLE_OTHER_INST(N,OPC,CLASS) toOtherOpCode N = Just OPC
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
toOtherOpCode _ = Nothing

toOpCode :: Integral a => a -> Maybe OpType
#define HANDLE_TERM_INST(N,OPC,CLASS) toOpCode N = Just (TermOp OPC)
#define HANDLE_BINARY_INST(N,OPC,CLASS) toOpCode N = Just (BinOp OPC)
#define HANDLE_MEMORY_INST(N,OPC,CLASS) toOpCode N = Just (MemoryOp OPC)
#define HANDLE_CAST_INST(N,OPC,CLASS) toOpCode N = Just (CastOp OPC)
#define HANDLE_OTHER_INST(N,OPC,CLASS) toOpCode N = Just (OtherOp OPC)
#if HS_LLVM_VERSION>=303
#include <llvm/IR/Instruction.def>
#else
#include <llvm/Instruction.def>
#endif
toOpCode _ = Nothing

binOpGetOpCode :: Ptr BinaryOperator -> IO BinOpType
binOpGetOpCode op = do
  opc <- binOpGetOpCode_ op
  let Just res = toBinOpCode opc
  return res

callInstGetNumArgOperands :: Ptr CallInst -> IO Integer
callInstGetNumArgOperands ptr = fmap toInteger (callInstGetNumArgOperands_ ptr)

callInstGetArgOperand :: Ptr CallInst -> Integer -> IO (Ptr Value)
callInstGetArgOperand ptr idx = callInstGetArgOperand_ ptr (fromInteger idx)

getFCmpOp :: Ptr FCmpInst -> IO FCmpOp
getFCmpOp ptr = fmap toFCmpOp (cmpInstGetPredicate_ ptr)

getICmpOp :: Ptr ICmpInst -> IO ICmpOp
getICmpOp ptr = fmap toICmpOp (cmpInstGetPredicate_ ptr)

inverseICmpOp :: ICmpOp -> ICmpOp
inverseICmpOp op = toICmpOp (cmpInstGetInversePredicate_ $ fromICmpOp op)

inverseFCmpOp :: FCmpOp -> FCmpOp
inverseFCmpOp op = toFCmpOp (cmpInstGetInversePredicate_ $ fromFCmpOp op)

swappedICmpOp :: ICmpOp -> ICmpOp
swappedICmpOp op = toICmpOp (cmpInstGetSwappedPredicate_ $ fromICmpOp op)

swappedFCmpOp :: FCmpOp -> FCmpOp
swappedFCmpOp op = toFCmpOp (cmpInstGetSwappedPredicate_ $ fromFCmpOp op)

TYPE(Instruction)
SUBTYPE2(Value,User,Instruction)
#if HS_LLVM_VERSION >= 300
TYPE_LEAF(AtomicCmpXchgInst)
SUBTYPE3(Value,User,Instruction,AtomicCmpXchgInst)
TYPE_LEAF(AtomicRMWInst)
SUBTYPE3(Value,User,Instruction,AtomicRMWInst)
#endif
TYPE_LEAF(BinaryOperator)
SUBTYPE3(Value,User,Instruction,BinaryOperator)
TYPE_LEAF(CallInst)
SUBTYPE3(Value,User,Instruction,CallInst)
TYPE(CmpInst)
SUBTYPE3(Value,User,Instruction,CmpInst)
TYPE_LEAF(FCmpInst)
SUBTYPE4(Value,User,Instruction,CmpInst,FCmpInst)
TYPE_LEAF(ICmpInst)
SUBTYPE4(Value,User,Instruction,CmpInst,ICmpInst)
TYPE_LEAF(ExtractElementInst)
SUBTYPE3(Value,User,Instruction,ExtractElementInst)
#if HS_LLVM_VERSION>=300
TYPE_LEAF(FenceInst)
SUBTYPE3(Value,User,Instruction,FenceInst)
#endif
TYPE_LEAF(GetElementPtrInst)
SUBTYPE3(Value,User,Instruction,GetElementPtrInst)
TYPE_LEAF(InsertElementInst)
SUBTYPE3(Value,User,Instruction,InsertElementInst)
TYPE_LEAF(InsertValueInst)
SUBTYPE3(Value,User,Instruction,InsertValueInst)
#if HS_LLVM_VERSION>=300
TYPE_LEAF(LandingPadInst)
SUBTYPE3(Value,User,Instruction,LandingPadInst)
#endif
TYPE_LEAF(PHINode)
SUBTYPE3(Value,User,Instruction,PHINode)
TYPE_LEAF(SelectInst)
SUBTYPE3(Value,User,Instruction,SelectInst)
TYPE_LEAF(ShuffleVectorInst)
SUBTYPE3(Value,User,Instruction,ShuffleVectorInst)
TYPE_LEAF(StoreInst)
SUBTYPE3(Value,User,Instruction,StoreInst)
TYPE(TerminatorInst)
SUBTYPE3(Value,User,Instruction,TerminatorInst)
TYPE_LEAF(BranchInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,BranchInst)
TYPE_LEAF(IndirectBrInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,IndirectBrInst)
TYPE_LEAF(InvokeInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,InvokeInst)
#if HS_LLVM_VERSION>=300
TYPE_LEAF(ResumeInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,ResumeInst)
#endif
TYPE_LEAF(ReturnInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,ReturnInst)
TYPE_LEAF(SwitchInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,SwitchInst)
TYPE_LEAF(UnreachableInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,UnreachableInst)
TYPE(UnaryInstruction)
SUBTYPE3(Value,User,Instruction,UnaryInstruction)
TYPE_LEAF(AllocaInst)
SUBTYPE4(Value,User,Instruction,UnaryInstruction,AllocaInst)
TYPE(CastInst)
SUBTYPE4(Value,User,Instruction,UnaryInstruction,CastInst)
TYPE_LEAF(BitCastInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,BitCastInst)
#if HS_LLVM_VERSION>=304
TYPE_LEAF(AddrSpaceCastInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,AddrSpaceCastInst)
#endif
TYPE_LEAF(FPExtInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,FPExtInst)
TYPE_LEAF(FPToSIInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,FPToSIInst)
TYPE_LEAF(FPToUIInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,FPToUIInst)
TYPE_LEAF(FPTruncInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,FPTruncInst)
TYPE_LEAF(IntToPtrInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,IntToPtrInst)
TYPE_LEAF(PtrToIntInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,PtrToIntInst)
TYPE_LEAF(SExtInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,SExtInst)
TYPE_LEAF(SIToFPInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,SIToFPInst)
TYPE_LEAF(TruncInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,TruncInst)
TYPE_LEAF(UIToFPInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,UIToFPInst)
TYPE_LEAF(ZExtInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,ZExtInst)
TYPE_LEAF(ExtractValueInst)
SUBTYPE4(Value,User,Instruction,UnaryInstruction,ExtractValueInst)
TYPE_LEAF(LoadInst)
SUBTYPE4(Value,User,Instruction,UnaryInstruction,LoadInst)
TYPE_LEAF(VAArgInst)
SUBTYPE4(Value,User,Instruction,UnaryInstruction,VAArgInst)

GETTYPE(Instruction)
#if HS_LLVM_VERSION >= 300
GETTYPE(AtomicCmpXchgInst)
GETTYPE(AtomicRMWInst)
#endif
GETTYPE(BinaryOperator)
GETTYPE(CallInst)
GETTYPE(CmpInst)
GETTYPE(FCmpInst)
GETTYPE(ICmpInst)
GETTYPE(ExtractElementInst)
#if HS_LLVM_VERSION >= 300
GETTYPE(FenceInst)
#endif

instance GetType GetElementPtrInst where
  type TypeOfValue GetElementPtrInst = PointerType
  getType = getElementPtrInstGetType

instance GetType InsertElementInst where
  type TypeOfValue InsertElementInst = VectorType
  getType = insertElementInstGetType

GETTYPE(InsertValueInst)
#if HS_LLVM_VERSION >= 300
GETTYPE(LandingPadInst)
#endif
GETTYPE(PHINode)
GETTYPE(SelectInst)

instance GetType ShuffleVectorInst where
  type TypeOfValue ShuffleVectorInst = VectorType
  getType = shuffleVectorInstGetType

GETTYPE(StoreInst)
GETTYPE(TerminatorInst)
GETTYPE(BranchInst)
GETTYPE(IndirectBrInst)
GETTYPE(InvokeInst)
#if HS_LLVM_VERSION >= 300
GETTYPE(ResumeInst)
#endif
GETTYPE(ReturnInst)
GETTYPE(SwitchInst)
GETTYPE(UnreachableInst)
GETTYPE(UnaryInstruction)

instance GetType AllocaInst where
  type TypeOfValue AllocaInst = PointerType
  getType = allocaInstGetType

GETTYPE(CastInst)
GETTYPE(BitCastInst)
#if HS_LLVM_VERSION >= 304
GETTYPE(AddrSpaceCastInst)
#endif
GETTYPE(FPExtInst)
GETTYPE(FPToUIInst)
GETTYPE(FPTruncInst)
GETTYPE(IntToPtrInst)
GETTYPE(PtrToIntInst)
GETTYPE(SExtInst)
GETTYPE(SIToFPInst)
GETTYPE(TruncInst)
GETTYPE(UIToFPInst)
GETTYPE(ZExtInst)
GETTYPE(ExtractValueInst)
GETTYPE(LoadInst)
GETTYPE(VAArgInst)
