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
         -- ** Binary Operator
         BinaryOperator(),
         binOpGetOpCode,
         -- ** Call Instructions
         CallInst(),
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
         getFCmpOp,
         -- *** Integer Compare Instruction
         ICmpInst(),
         ICmpOp(..),
         getICmpOp,
         -- ** Extract Element Instruction
         ExtractElementInst(),
         -- ** Fence Instruction
         FenceInst(),
         fenceInstGetOrdering,
         -- ** Get Element Pointer Instruction
         GetElementPtrInst(),
         getElementPtrInstGetPointerOperand,
         getElementPtrInstIdxBegin,
         getElementPtrInstIdxEnd,
         getElementPtrInstIsInBounds,
         getElementPtrInstGetNumIndices,
         -- ** Insert Element Instruction
         InsertElementInst(),
         -- ** Insert Value Instruction
         InsertValueInst(),
         -- ** Landing Pad Instruction
         LandingPadInst(),
         landingPadInstGetPersonaliteFn,
         landingPadInstIsCleanup,
         landingPadInstGetNumClauses,
         landingPadInstGetClause,
         landingPadInstIsCatch,
         landingPadInstIsFilter,
         -- ** PHI-Node
         PHINode(),
         phiNodeGetNumIncomingValues,
         phiNodeGetIncomingValue,
         phiNodeGetIncomingBlock,
         -- ** Selection Instruction
         SelectInst(),
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
         invokeInstGetLandingPadInst,
         -- *** Resume Instruction
         ResumeInst(),
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

import Foreign
import Foreign.C

#include "Helper.h"

SPECIALIZE_IPLIST(Instruction,capi)

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

data SynchronizationScope =
#define HANDLE_SYNC_SCOPE(name) PRESERVE(  ) name
#define HANDLE_SEP PRESERVE(  ) |
#include "SyncScope.def"
  deriving (Show,Eq,Ord)

#define HANDLE_SYNC_SCOPE(name) foreign import capi _TO_STRING(extra.h SynchronizationScope_##name) synchronizationScope_##name :: CInt
#include "SyncScope.def"

toSynchronizationScope :: CInt -> SynchronizationScope
toSynchronizationScope op
#define HANDLE_SYNC_SCOPE(name) PRESERVE(  ) | op == synchronizationScope_##name = name
#include "SyncScope.def"

fromSynchronizationScope :: SynchronizationScope -> CInt
#define HANDLE_SYNC_SCOPE(name) fromSynchronizationScope name = synchronizationScope_##name
#include "SyncScope.def"

#if HS_LLVM_VERSION >= 303
isMallocLikeFn :: ValueC t => Ptr t -> Ptr TargetLibraryInfo -> Bool -> IO Bool
#else
isMallocLikeFn :: ValueC t => Ptr t -> IO Bool
#endif
isMallocLikeFn = isMallocLikeFn_

loadInstGetOrdering :: Ptr LoadInst -> IO AtomicOrdering
loadInstGetOrdering = fmap toAtomicOrdering . loadInstGetOrdering_

storeInstGetOrdering :: Ptr StoreInst -> IO AtomicOrdering
storeInstGetOrdering = fmap toAtomicOrdering . storeInstGetOrdering_

atomicRMWInstGetOperation :: Ptr AtomicRMWInst -> IO RMWBinOp
atomicRMWInstGetOperation = fmap toRMWBinOp . atomicRMWInstGetOperation_

atomicRMWInstGetOrdering :: Ptr AtomicRMWInst -> IO AtomicOrdering
atomicRMWInstGetOrdering = fmap toAtomicOrdering . atomicRMWInstGetOrdering_

data RMWBinOp =
#define HANDLE_BINOP(name) PRESERVE(  ) RMW##name
#define HANDLE_SEP PRESERVE(  ) |
#include "RMWBinOp.def"
  deriving (Show,Eq,Ord)

#define HANDLE_BINOP(name) foreign import capi _TO_STRING(extra.h RMWBinOp_##name) rmwBinOp_##name :: CInt
#include "RMWBinOp.def"

toRMWBinOp :: CInt -> RMWBinOp
toRMWBinOp op
#define HANDLE_BINOP(name) PRESERVE(  ) | op == rmwBinOp_##name = RMW##name
#include "RMWBinOp.def"

fromRMWBinOp :: RMWBinOp -> CInt
#define HANDLE_BINOP(name) fromRMWBinOp RMW##name = rmwBinOp_##name
#include "RMWBinOp.def"

atomicCmpXchgInstGetOrdering :: Ptr AtomicCmpXchgInst -> IO AtomicOrdering
atomicCmpXchgInstGetOrdering = fmap toAtomicOrdering . atomicCmpXchgInstGetOrdering_

fenceInstGetOrdering :: Ptr FenceInst -> IO AtomicOrdering
fenceInstGetOrdering = fmap toAtomicOrdering . fenceInstGetOrdering_

data AtomicOrdering =
#define HANDLE_ORDERING(name) PRESERVE(  ) name
#define HANDLE_SEP PRESERVE(  ) |
#include "AtomicOrdering.def"
  deriving (Show,Eq,Ord)

#define HANDLE_ORDERING(name) foreign import capi _TO_STRING(extra.h AtomicOrdering_##name) atomicOrdering_##name :: CInt
#include "AtomicOrdering.def"

toAtomicOrdering :: CInt -> AtomicOrdering
toAtomicOrdering op
#define HANDLE_ORDERING(name) PRESERVE(  ) | op == atomicOrdering_##name = name
#include "AtomicOrdering.def"

fromAtomicOrdering :: AtomicOrdering -> CInt
#define HANDLE_ORDERING(name) fromAtomicOrdering name = atomicOrdering_##name
#include "AtomicOrdering.def"

landingPadInstIsCatch :: Ptr LandingPadInst -> Integer -> IO Bool
landingPadInstIsCatch ptr i = landingPadInstIsCatch_ ptr (fromInteger i)

landingPadInstIsFilter :: Ptr LandingPadInst -> Integer -> IO Bool
landingPadInstIsFilter ptr i = landingPadInstIsCatch_ ptr (fromInteger i)

landingPadInstGetClause :: Ptr LandingPadInst -> Integer -> IO (Ptr Value)
landingPadInstGetClause ptr i = landingPadInstGetClause_ ptr (fromInteger i)

landingPadInstGetNumClauses :: Ptr LandingPadInst -> IO Integer
landingPadInstGetNumClauses = fmap toInteger . landingPadInstGetNumClauses_

callInstGetCallingConv :: Ptr CallInst -> IO CallingConv
callInstGetCallingConv = fmap toCallingConv . callInstGetCallingConv_

invokeInstGetCallingConv :: Ptr InvokeInst -> IO CallingConv
invokeInstGetCallingConv = fmap toCallingConv . invokeInstGetCallingConv_

data CallingConv =
#define HANDLE_CC(name) PRESERVE(  ) name
#define HANDLE_SEP PRESERVE(  ) |
#include "CConvs.def"
  deriving (Show,Eq,Ord)

#define HANDLE_CC(name) foreign import capi _TO_STRING(extra.h CConv_##name) cConv_##name :: CInt
#include "CConvs.def"

toCallingConv :: CInt -> CallingConv
toCallingConv op
#define HANDLE_CC(name) PRESERVE(  ) | op == cConv_##name = name
#include "CConvs.def"

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

foreign import capi "extra.h FCMP_OEQ"
  fCMP_OEQ :: CInt
foreign import capi "extra.h FCMP_OGT"
  fCMP_OGT :: CInt
foreign import capi "extra.h FCMP_OGE"
  fCMP_OGE :: CInt
foreign import capi "extra.h FCMP_OLT"
  fCMP_OLT :: CInt
foreign import capi "extra.h FCMP_OLE"
  fCMP_OLE :: CInt
foreign import capi "extra.h FCMP_ONE"
  fCMP_ONE :: CInt
foreign import capi "extra.h FCMP_ORD"
  fCMP_ORD :: CInt
foreign import capi "extra.h FCMP_UNO"
  fCMP_UNO :: CInt
foreign import capi "extra.h FCMP_UEQ"
  fCMP_UEQ :: CInt
foreign import capi "extra.h FCMP_UGT"
  fCMP_UGT :: CInt
foreign import capi "extra.h FCMP_UGE"
  fCMP_UGE :: CInt
foreign import capi "extra.h FCMP_ULT"
  fCMP_ULT :: CInt
foreign import capi "extra.h FCMP_ULE"
  fCMP_ULE :: CInt
foreign import capi "extra.h FCMP_UNE"
  fCMP_UNE :: CInt
foreign import capi "extra.h ICMP_EQ"
  iCMP_EQ :: CInt
foreign import capi "extra.h ICMP_NE"
  iCMP_NE :: CInt
foreign import capi "extra.h ICMP_UGT"
  iCMP_UGT :: CInt
foreign import capi "extra.h ICMP_UGE"
  iCMP_UGE :: CInt
foreign import capi "extra.h ICMP_ULT"
  iCMP_ULT :: CInt
foreign import capi "extra.h ICMP_ULE"
  iCMP_ULE :: CInt
foreign import capi "extra.h ICMP_SGT"
  iCMP_SGT :: CInt
foreign import capi "extra.h ICMP_SGE"
  iCMP_SGE :: CInt
foreign import capi "extra.h ICMP_SLT"
  iCMP_SLT :: CInt
foreign import capi "extra.h ICMP_SLE"
  iCMP_SLE :: CInt

data FCmpOp = F_OEQ
            | F_OGT
            | F_OGE
            | F_OLT
            | F_OLE
            | F_ONE
            | F_ORD
            | F_UNO
            | F_UEQ
            | F_UGT
            | F_UGE
            | F_ULT
            | F_ULE
            | F_UNE
            deriving (Show,Eq,Ord)

data ICmpOp = I_EQ
            | I_NE
            | I_UGT
            | I_UGE
            | I_ULT
            | I_ULE
            | I_SGT
            | I_SGE
            | I_SLT
            | I_SLE
            deriving (Show,Eq,Ord)

toFCmpOp :: CInt -> FCmpOp
toFCmpOp op
  | op == fCMP_OEQ = F_OEQ
  | op == fCMP_OGT = F_OGT
  | op == fCMP_OGE = F_OGE
  | op == fCMP_OLT = F_OLT
  | op == fCMP_OLE = F_OLE
  | op == fCMP_ONE = F_ONE
  | op == fCMP_ORD = F_ORD
  | op == fCMP_UNO = F_UNO
  | op == fCMP_UEQ = F_UEQ
  | op == fCMP_UGT = F_UGT
  | op == fCMP_UGE = F_UGE
  | op == fCMP_ULT = F_ULT
  | op == fCMP_ULE = F_ULE
  | op == fCMP_UNE = F_UNE

toICmpOp :: CInt -> ICmpOp
toICmpOp op
  | op == iCMP_EQ = I_EQ
  | op == iCMP_NE = I_NE
  | op == iCMP_UGT = I_UGT
  | op == iCMP_UGE = I_UGE
  | op == iCMP_ULT = I_ULT
  | op == iCMP_ULE = I_ULE
  | op == iCMP_SGT = I_SGT
  | op == iCMP_SGE = I_SGE
  | op == iCMP_SLT = I_SLT
  | op == iCMP_SLE = I_SLE

getFCmpOp :: Ptr FCmpInst -> IO FCmpOp
getFCmpOp ptr = fmap toFCmpOp (cmpInstGetPredicate_ ptr)

getICmpOp :: Ptr ICmpInst -> IO ICmpOp
getICmpOp ptr = fmap toICmpOp (cmpInstGetPredicate_ ptr)

TYPE(Instruction)
SUBTYPE2(Value,User,Instruction)
TYPE_LEAF(AtomicCmpXchgInst)
SUBTYPE3(Value,User,Instruction,AtomicCmpXchgInst)
TYPE_LEAF(AtomicRMWInst)
SUBTYPE3(Value,User,Instruction,AtomicRMWInst)
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
TYPE_LEAF(FenceInst)
SUBTYPE3(Value,User,Instruction,FenceInst)
TYPE_LEAF(GetElementPtrInst)
SUBTYPE3(Value,User,Instruction,GetElementPtrInst)
TYPE_LEAF(InsertElementInst)
SUBTYPE3(Value,User,Instruction,InsertElementInst)
TYPE_LEAF(InsertValueInst)
SUBTYPE3(Value,User,Instruction,InsertValueInst)
TYPE_LEAF(LandingPadInst)
SUBTYPE3(Value,User,Instruction,LandingPadInst)
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
TYPE_LEAF(ResumeInst)
SUBTYPE4(Value,User,Instruction,TerminatorInst,ResumeInst)
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
GETTYPE(AtomicCmpXchgInst)
GETTYPE(AtomicRMWInst)
GETTYPE(BinaryOperator)
GETTYPE(CallInst)
GETTYPE(CmpInst)
GETTYPE(FCmpInst)
GETTYPE(ICmpInst)
GETTYPE(ExtractElementInst)
GETTYPE(FenceInst)

instance GetType GetElementPtrInst where
  type TypeOfValue GetElementPtrInst = PointerType
  getType = getElementPtrInstGetType

instance GetType InsertElementInst where
  type TypeOfValue InsertElementInst = VectorType
  getType = insertElementInstGetType

GETTYPE(InsertValueInst)
GETTYPE(LandingPadInst)
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
GETTYPE(ResumeInst)
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
