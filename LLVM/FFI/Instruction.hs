module LLVM.FFI.Instruction 
       (-- * Instructions
         Instruction(),
         InstructionC(),
         instructionGetParent,
         instructionGetDebugLoc,
         -- ** Atomic Compare & Exchange Instruction
         AtomicCmpXchgInst(),
         -- ** Atomic Read & Modify & Write Instruction
         AtomicRMWInst(),
         -- ** Binary Operator
         BinaryOperator(),
         BinOpType(..),
         binOpGetOpCode,
         -- ** Call Instructions
         CallInst(),
         callInstIsTailCall,
         callInstGetNumArgOperands,
         callInstGetArgOperand,
         callInstGetCalledValue,
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
         -- ** Get Element Pointer Instruction
         GetElementPtrInst(),
         -- ** Insert Element Instruction
         InsertElementInst(),
         -- ** Insert Value Instruction
         InsertValueInst(),
         -- ** Landing Pad Instruction
         LandingPadInst(),
         -- ** PHI-Node
         PHINode(),
         phiNodeGetNumIncomingValues,
         phiNodeGetIncomingValue,
         phiNodeGetIncomingBlock,
         -- ** Selection Instruction
         SelectInst(),
         -- ** Shuffle Vector Instruction
         ShuffleVectorInst(),
         -- ** Store Instruction
         StoreInst(),
         -- ** Terminator Instructions
         TerminatorInst(),
         TerminatorInstC(),
         -- *** Branch Instruction
         BranchInst(),
         -- *** Indirect Branch Instruction
         IndirectBrInst(),
         -- *** Invoke Instruction
         InvokeInst(),
         -- *** Resume Instruction
         ResumeInst(),
         -- *** Return Instruction
         ReturnInst(),
         -- *** Switch Instruction
         SwitchInst(),
         -- *** Unreachable Instruction
         UnreachableInst(),
         -- ** Unary Instructions
         UnaryInstruction(),
         UnaryInstructionC(),
         -- *** Allocation Instruction
         AllocaInst(),
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

phiNodeGetNumIncomingValues :: Ptr PHINode -> IO Integer
phiNodeGetNumIncomingValues ptr = fmap toInteger (phiNodeGetNumIncomingValues_ ptr)

phiNodeGetIncomingValue :: Ptr PHINode -> Integer -> IO (Ptr Value)
phiNodeGetIncomingValue ptr idx = phiNodeGetIncomingValue_ ptr (fromInteger idx)

phiNodeGetIncomingBlock :: Ptr PHINode -> Integer -> IO (Ptr BasicBlock)
phiNodeGetIncomingBlock ptr idx = phiNodeGetIncomingBlock_ ptr (fromInteger idx)

data BinOpType =
#define HANDLE_BINARY_INST(N,OPC,CLASS) PRESERVE(  ) OPC |
#include <llvm/Instruction.def>
  UnknownBinOp
  deriving Show

toOpCode :: CInt -> BinOpType
#define HANDLE_BINARY_INST(N,OPC,CLASS) toOpCode N = OPC
#include <llvm/Instruction.def>
toOpCode _ = UnknownBinOp

binOpGetOpCode :: Ptr BinaryOperator -> IO BinOpType
binOpGetOpCode op = fmap toOpCode (binOpGetOpCode_ op)

callInstGetNumArgOperands :: Ptr CallInst -> IO Integer
callInstGetNumArgOperands ptr = fmap toInteger (callInstGetNumArgOperands_ ptr)

callInstGetArgOperand :: Ptr CallInst -> Integer -> IO (Ptr Value)
callInstGetArgOperand ptr idx = callInstGetArgOperand ptr (fromInteger idx)

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

getICmpOp :: Ptr FCmpInst -> IO ICmpOp
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
