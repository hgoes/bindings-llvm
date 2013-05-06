#if HS_LLVM_VERSION >= 303
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/DataLayout.h>
#elif HS_LLVM_VERSION == 302
#include <llvm/InstrTypes.h>
#include <llvm/Instructions.h>
#include <llvm/CallingConv.h>
#include <llvm/DataLayout.h>
#else
#include <llvm/InstrTypes.h>
#include <llvm/Instructions.h>
#include <llvm/CallingConv.h>
#include <llvm/Target/TargetData.h>
#endif
#include <llvm/Support/raw_ostream.h>
#include <llvm/Bitcode/ReaderWriter.h>
#if HS_LLVM_VERSION >= 209
#include <llvm/Target/TargetLibraryInfo.h>
#endif
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/FindUsedTypes.h>

extern "C" {
  char* passId_LoopInfo() { return &llvm::LoopInfo::ID; }
  char* passId_FindUsedTypes() { return &llvm::FindUsedTypes::ID; }
#if HS_LLVM_VERSION >= 209
  char* passId_TargetLibraryInfo() { return &llvm::TargetLibraryInfo::ID; }
#endif
#if HS_LLVM_VERSION >= 302
  char* passId_DataLayout() { return &llvm::DataLayout::ID; }
#else
  char* passId_TargetData() { return &llvm::TargetData::ID; }
#endif

  int writeBitCodeToFile(void* m,const char* path) {
    std::string ErrorInfo;
    llvm::raw_fd_ostream OS(path, ErrorInfo, llvm::raw_fd_ostream::F_Binary);
    if (!ErrorInfo.empty())
      return -1;
    llvm::WriteBitcodeToFile((llvm::Module*)m, OS);
    return 0;
  }
}
