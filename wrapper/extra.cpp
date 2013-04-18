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
#define HANDLE_FPRED(name) int FCMP_##name() { return llvm::CmpInst::FCMP_##name; }
#define HANDLE_IPRED(name) int ICMP_##name() { return llvm::CmpInst::ICMP_##name; }
#include "Predicate.def"

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
