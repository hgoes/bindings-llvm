#if HS_LLVM_VERSION >= 303
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#else
#include <llvm/InstrTypes.h>
#include <llvm/Instructions.h>
#include <llvm/CallingConv.h>
#endif
#include <llvm/Support/raw_ostream.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Target/TargetLibraryInfo.h>
#include <llvm/Analysis/AliasAnalysis.h>

extern "C" {
  int FCMP_OEQ() {
    return llvm::CmpInst::FCMP_OEQ;
  }
  int FCMP_OGT() {
    return llvm::CmpInst::FCMP_OGT;
  }
  int FCMP_OGE() {
    return llvm::CmpInst::FCMP_OGE;
  }
  int FCMP_OLT() {
    return llvm::CmpInst::FCMP_OLT;
  }
  int FCMP_OLE() {
    return llvm::CmpInst::FCMP_OLE;
  }
  int FCMP_ONE() {
    return llvm::CmpInst::FCMP_ONE;
  }
  int FCMP_ORD() {
    return llvm::CmpInst::FCMP_ORD;
  }
  int FCMP_UNO() {
    return llvm::CmpInst::FCMP_UNO;
  }
  int FCMP_UEQ() {
    return llvm::CmpInst::FCMP_UEQ;
  }
  int FCMP_UGT() {
    return llvm::CmpInst::FCMP_UGT;
  }
  int FCMP_UGE() {
    return llvm::CmpInst::FCMP_UGE;
  }
  int FCMP_ULT() {
    return llvm::CmpInst::FCMP_ULT;
  }
  int FCMP_ULE() {
    return llvm::CmpInst::FCMP_ULE;
  }
  int FCMP_UNE() {
    return llvm::CmpInst::FCMP_UNE;
  }
  int ICMP_EQ() {
    return llvm::CmpInst::ICMP_EQ;
  }
  int ICMP_NE() {
    return llvm::CmpInst::ICMP_NE;
  }
  int ICMP_UGT() {
    return llvm::CmpInst::ICMP_UGT;
  }
  int ICMP_UGE() {
    return llvm::CmpInst::ICMP_UGE;
  }
  int ICMP_ULT() {
    return llvm::CmpInst::ICMP_ULT;
  }
  int ICMP_ULE() {
    return llvm::CmpInst::ICMP_ULE;
  }
  int ICMP_SGT() {
    return llvm::CmpInst::ICMP_SGT;
  }
  int ICMP_SGE() {
    return llvm::CmpInst::ICMP_SGE;
  }
  int ICMP_SLT() {
    return llvm::CmpInst::ICMP_SLT;
  }
  int ICMP_SLE() {
    return llvm::CmpInst::ICMP_SLE;
  }

#define HANDLE_CC(name) int CConv_##name() { return llvm::CallingConv::name; }
#include "CConvs.def"

#define HANDLE_ORDERING(name) int AtomicOrdering_##name() { return llvm::name; }
#include "AtomicOrdering.def"

#define HANDLE_BINOP(name) int RMWBinOp_##name() { return llvm::AtomicRMWInst::name; }
#include "RMWBinOp.def"

#define HANDLE_LIBFUNC(name) int LibFunc_##name() { return llvm::LibFunc::name; }
#include "LibFunc.def"

#define HANDLE_ALIAS_RESULT(name) int AliasResult_##name() { return llvm::AliasAnalysis::name; }
#include "Alias.def"

#define HANDLE_SYNC_SCOPE(name) int SynchronizationScope_##name() { return llvm::name; }
#include "SyncScope.def"

  int writeBitCodeToFile(void* m,const char* path) {
    std::string ErrorInfo;
    llvm::raw_fd_ostream OS(path, ErrorInfo, llvm::raw_fd_ostream::F_Binary);
    if (!ErrorInfo.empty())
      return -1;
    llvm::WriteBitcodeToFile((llvm::Module*)m, OS);
    return 0;
  }
}
