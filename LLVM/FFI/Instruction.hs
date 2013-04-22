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
         AtomicOrdering(..),
         toAtomicOrdering,
         RMWBinOp(..),
         toRMWBinOp,
         SynchronizationScope(..),
         toSynchronizationScope,
         instructionGetParent,
         instructionGetDebugLoc,
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
         -- *** Float Compare Instruction
         FCmpInst(),
         FCmpOp(..),
         newFCmpInst,
         getFCmpOp,
         -- *** Integer Compare Instruction
         ICmpInst(),
         ICmpOp(..),
         newICmpInst,
         getICmpOp,
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
         -- ** Store Instruction
         StoreInst(),
         storeInstIsVolatile,
         storeInstGetAlignment,
         storeInstGetOrdering,
         storeInstGetValueOperand,
         storeInstGetPointerOperand,
         -- ** Terminator Instructions
         TerminatorInst(),
         TerminatorInstC(),
         terminatorInstGetNumSuccessors,
         terminatorInstGetSuccessor,
         -- *** Branch Instruction
         BranchInst(),
         branchInstIsConditional,
         branchInstGetCondition,
         -- *** Indirect Branch Instruction
         IndirectBrInst(),
         -- *** Invoke Instruction
         InvokeInst(),
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
#endif
         -- *** Return Instruction
         ReturnInst(),
         returnInstGetReturnValue,
         -- *** Switch Instruction
         SwitchInst(),
#if HS_LLVM_VERSION>=301
         CaseIt(),
#endif
         switchInstGetCondition,
#if HS_LLVM_VERSION>=301
         switchInstCaseBegin,
         switchInstCaseEnd,
         switchInstCaseDefault,
         caseItNext,
         caseItPrev,
         caseItEq,
         caseItGetCaseValue,
         caseItGetCaseSuccessor,
#endif
         -- *** Unreachable Instruction
         UnreachableInst(),
         -- ** Unary Instructions
         UnaryInstruction(),
         UnaryInstructionC(),
         -- *** Allocation Instruction
         AllocaInst(),
         allocaInstIsArrayAllocation,
         allocaInstGetArraySize,
         allocaInstGetAlignment,
         -- *** Casting Instructions
         CastInst(),
         CastInstC(),
         -- **** Bitcasting Instruction
         BitCastInst(),
         -- **** Floating Point Extend Instruction
         FPExtInst(),
         -- **** Floating Point to Unsigned Integer Instruction
         FPToUIInst(),
         -- **** Floatting Point Truncation Instruction
         FPTruncInst(),
         -- **** Integer to Pointer Instruction
         IntToPtrInst(),
         -- **** Pointer to Integer Instruction
         PtrToIntInst(),
         -- **** Signed Extend Instruction
         SExtInst(),
         -- **** Signed Integer to Floating Point Instruction
         SIToFPInst(),
         -- **** Truncation Instruction
         TruncInst(),
         -- **** Unsigned Integer to Floating Point Instruction
         UIToFPInst(),
         -- **** Zero Extend Instrruction
         ZExtInst(),
         -- *** ExtractValue Instruction
         ExtractValueInst(),
         -- *** Loading Instruction
         LoadInst(),
         loadInstIsVolatile,
         loadInstGetAlignment,
         loadInstGetOrdering,
         loadInstGetPointerOperand,
         -- *** VarArg Instruction
         VAArgInst()
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

newSelectInst :: (ValueC c,ValueC s1,ValueC s2) => Ptr c -> Ptr s1 -> Ptr s2 -> Ptr Twine -> IO (Ptr SelectInst)
newSelectInst = newSelectInst_

phiNodeAddIncoming :: ValueC val => Ptr PHINode -> Ptr val -> Ptr BasicBlock -> IO ()
phiNodeAddIncoming = phiNodeAddIncoming_

newPhiNode :: TypeC tp => Ptr tp -> CUInt -> Ptr Twine -> IO (Ptr PHINode)
newPhiNode = newPhiNode_

#if HS_LLVM_VERSION>=300
landingPadInstAddClause :: ValueC clause => Ptr LandingPadInst -> Ptr clause -> IO ()
landingPadInstAddClause = landingPadInstAddClause_

newLandingPadInst :: (TypeC tp,ValueC fn) => Ptr tp -> Ptr fn -> CUInt -> Ptr Twine -> IO (Ptr LandingPadInst)
newLandingPadInst = newLandingPadInst_
#endif

newInsertValueInst :: (ValueC agg,ValueC val) => Ptr agg -> Ptr val -> Ptr (ArrayRef CUInt) -> Ptr Twine -> IO (Ptr InsertValueInst)
newInsertValueInst = newInsertValueInst_

newInsertElementInst :: (ValueC vec,ValueC newEl,ValueC idx) => Ptr vec -> Ptr newEl -> Ptr idx -> Ptr Twine -> IO (Ptr InsertElementInst)
newInsertElementInst = newInsertElementInst_

newGetElementPtrInst :: ValueC ptr => Ptr ptr -> Ptr (ArrayRef (Ptr Value)) -> Ptr Twine -> IO (Ptr GetElementPtrInst)
newGetElementPtrInst = newGetElementPtrInst_

#if HS_LLVM_VERSION>=300
newFenceInst :: Ptr LLVMContext -> AtomicOrdering -> SynchronizationScope -> IO (Ptr FenceInst)
newFenceInst ctx ord sync = newFenceInst_ ctx (fromAtomicOrdering ord) (fromSynchronizationScope sync)
#endif

newExtractElementInst :: (ValueC vec,ValueC idx) => Ptr vec -> Ptr idx -> Ptr Twine -> IO (Ptr ExtractElementInst)
newExtractElementInst = newExtractElementInst_

newICmpInst :: (ValueC v1,ValueC v2) => ICmpOp -> Ptr v1 -> Ptr v2 -> Ptr Twine -> IO (Ptr ICmpInst)
newICmpInst op = newICmpInst_ (fromICmpOp op)

newFCmpInst :: (ValueC v1,ValueC v2) => FCmpOp -> Ptr v1 -> Ptr v2 -> Ptr Twine -> IO (Ptr FCmpInst)
newFCmpInst op = newFCmpInst_ (fromFCmpOp op)

newCallInst :: ValueC fun => Ptr fun -> Ptr (ArrayRef (Ptr Value)) -> Ptr Twine -> IO (Ptr CallInst)
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

loadInstGetOrdering :: Ptr LoadInst -> IO AtomicOrdering
loadInstGetOrdering = fmap toAtomicOrdering . loadInstGetOrdering_

storeInstGetOrdering :: Ptr StoreInst -> IO AtomicOrdering
storeInstGetOrdering = fmap toAtomicOrdering . storeInstGetOrdering_

#if HS_LLVM_VERSION >= 300
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

getElementPtrInstGetNumIndices :: Ptr GetElementPtrInst -> IO Integer
getElementPtrInstGetNumIndices ptr = fmap toInteger (getElementPtrInstGetNumIndices_ ptr)

loadInstGetAlignment :: Ptr LoadInst -> IO Integer
loadInstGetAlignment ptr = fmap toInteger (loadInstGetAlignment_ ptr)

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

data OpType 
  = TermOp TermOpType
  | BinOp BinOpType
  | MemoryOp MemoryOpType
  | CastOp CastOpType
  | OtherOp OtherOpType
  deriving (Show,Eq,Ord)

data TermOpType =
#define HANDLE_TERM_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#include <llvm/Instruction.def>
  UnknownTermOp
  deriving (Show,Eq,Ord)

data BinOpType =
#define HANDLE_BINARY_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#include <llvm/Instruction.def>
  UnknownBinOp
  deriving (Show,Eq,Ord)

data MemoryOpType =
#define HANDLE_MEMORY_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#include <llvm/Instruction.def>
  UnknownMemoryOp
  deriving (Show,Eq,Ord)

data CastOpType =
#define HANDLE_CAST_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#include <llvm/Instruction.def>
  UnknownCastOp
  deriving (Show,Eq,Ord)

data OtherOpType =
#define HANDLE_OTHER_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#include <llvm/Instruction.def>
  UnknownOtherOp
  deriving (Show,Eq,Ord)

toTermOpCode :: Integral a => a -> Maybe TermOpType
#define HANDLE_TERM_INST(N,OPC,CLASS) toTermOpCode N = Just OPC
#include <llvm/Instruction.def>
toTermOpCode _ = Nothing

toBinOpCode :: Integral a => a -> Maybe BinOpType
#define HANDLE_BINARY_INST(N,OPC,CLASS) toBinOpCode N = Just OPC
#include <llvm/Instruction.def>
toBinOpCode _ = Nothing

fromBinOpCode :: BinOpType -> CInt
#define HANDLE_BINARY_INST(N,OPC,CLASS) fromBinOpCode OPC = N
#include <llvm/Instruction.def>

toMemoryOpCode :: Integral a => a -> Maybe MemoryOpType
#define HANDLE_MEMORY_INST(N,OPC,CLASS) toMemoryOpCode N = Just OPC
#include <llvm/Instruction.def>
toMemoryOpCode _ = Nothing

toCastOpCode :: Integral a => a -> Maybe CastOpType
#define HANDLE_CAST_INST(N,OPC,CLASS) toCastOpCode N = Just OPC
#include <llvm/Instruction.def>
toCastOpCode _ = Nothing

toOtherOpCode :: Integral a => a -> Maybe OtherOpType
#define HANDLE_OTHER_INST(N,OPC,CLASS) toOtherOpCode N = Just OPC
#include <llvm/Instruction.def>
toOtherOpCode _ = Nothing

toOpCode :: Integral a => a -> Maybe OpType
#define HANDLE_TERM_INST(N,OPC,CLASS) toOpCode N = Just (TermOp OPC)
#define HANDLE_BINARY_INST(N,OPC,CLASS) toOpCode N = Just (BinOp OPC)
#define HANDLE_MEMORY_INST(N,OPC,CLASS) toOpCode N = Just (MemoryOp OPC)
#define HANDLE_CAST_INST(N,OPC,CLASS) toOpCode N = Just (CastOp OPC)
#define HANDLE_OTHER_INST(N,OPC,CLASS) toOpCode N = Just (OtherOp OPC)
#include <llvm/Instruction.def>
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
TYPE_LEAF(FPExtInst)
SUBTYPE5(Value,User,Instruction,UnaryInstruction,CastInst,FPExtInst)
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
