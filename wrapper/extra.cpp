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
#if HS_LLVM_VERSION >= 209
#include <llvm/Support/FileSystem.h>
#endif
#include <llvm/Bitcode/ReaderWriter.h>
#if HS_LLVM_VERSION>=307
#include <llvm/Analysis/TargetLibraryInfo.h>
#elif HS_LLVM_VERSION >= 209
#include <llvm/Target/TargetLibraryInfo.h>
#endif
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/LoopInfo.h>
#if HS_LLVM_VERSION<306
#include <llvm/Analysis/FindUsedTypes.h>
#endif
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ADT/APInt.h>
#if HS_LLVM_VERSION>=305
#include <llvm/IR/Dominators.h>
#endif
#include <llvm/Analysis/ScalarEvolution.h>

#include <string>

extern "C" {
#if HS_LLVM_VERSION<307
  char* passId_LoopInfo() { return &llvm::LoopInfo::ID; }
#endif
  //char* passId_FindUsedTypes() { return &llvm::FindUsedTypes::ID; }
#if HS_LLVM_VERSION<306
  const char* passId_FindUsedTypes = &llvm::FindUsedTypes::ID;
#endif
#if HS_LLVM_VERSION >= 209 && HS_LLVM_VERSION<307
  char* passId_TargetLibraryInfo() { return &llvm::TargetLibraryInfo::ID; }
#endif
#if HS_LLVM_VERSION < 307
#if HS_LLVM_VERSION < 302
  char* passId_TargetData() { return &llvm::TargetData::ID; }
#elif HS_LLVM_VERSION < 305
  char* passId_DataLayout() { return &llvm::DataLayout::ID; }
#else
  char* passId_DataLayoutPass() { return &llvm::DataLayoutPass::ID; }
#endif
#endif
#if HS_LLVM_VERSION < 305
  char* passId_DominatorTree() { return &llvm::DominatorTree::ID; }
#else
  char* passId_DominatorTreeWrapperPass() { return &llvm::DominatorTreeWrapperPass::ID; }
#endif
  char* passId_AliasAnalysis() { return &llvm::AliasAnalysis::ID; }
  char* passId_ScalarEvolution() { return &llvm::ScalarEvolution::ID; }
  
  int writeBitCodeToFile(void* m,const char* path) {
#if HS_LLVM_VERSION < 306
    std::string ErrorInfo;
#else
    std::error_code ErrorInfo;
#endif
#if HS_LLVM_VERSION <= 303
    llvm::raw_fd_ostream OS(path, ErrorInfo, llvm::raw_fd_ostream::F_Binary);
#elif HS_LLVM_VERSION < 305
    llvm::raw_fd_ostream OS(path, ErrorInfo, llvm::sys::fs::F_None);
#elif HS_LLVM_VERSION < 306
    llvm::raw_fd_ostream OS(path, ErrorInfo, llvm::sys::fs::OpenFlags::F_None);
#else
    llvm::raw_fd_ostream OS(path, ErrorInfo, llvm::sys::fs::OpenFlags::F_None);
#endif
#if HS_LLVM_VERSION < 306
    if (!ErrorInfo.empty())
      return -1;
#else
    if (ErrorInfo)
      return -1;
#endif
    llvm::WriteBitcodeToFile((llvm::Module*)m, OS);
    return 0;
  }

  void* std_string_empty() {
    return (void*)new std::string();
  }

  void* std_string_from_string(char* str) {
    return (void*)new std::string(str);
  }

  void std_string_delete(void* str) {
    delete (std::string*)str;
  }

  const char* std_string_to_string(void* str) {
    return ((std::string*)str)->c_str();
  }

  char* value_to_string(void* val) {
    std::string outp;
    llvm::raw_string_ostream stream(outp);
    ((llvm::Value*)val)->print(stream);
    return strdup(stream.str().c_str());
  }

  size_t sizeof_APInt = sizeof(llvm::APInt);
  size_t alignof_APInt = __alignof__(llvm::APInt);

  size_t sizeof_GenericValue = sizeof(llvm::GenericValue);
  size_t alignof_GenericValue = __alignof__(llvm::GenericValue);

  void move_APInt(void* trg,unsigned bw,unsigned wcount,uint64_t* arr) {
#if HS_LLVM_VERSION>=300
    *((llvm::APInt*)trg) = llvm::APInt(bw,llvm::ArrayRef<uint64_t>(arr,wcount));
#else
    *((llvm::APInt*)trg) = llvm::APInt(bw,wcount,arr);
#endif
  }

  void move_APIntSimple(void* trg,unsigned bw,uint64_t val) {
    *((llvm::APInt*)trg) = llvm::APInt(bw,val,false);
  }

  void genericValueSetIntPair(void* val,unsigned i1,unsigned i2) {
    llvm::GenericValue* res = (llvm::GenericValue*)val;
    res->UIntPairVal.first = i1;
    res->UIntPairVal.second = i2;
  }

  void genericValueGetIntPair(void* val,unsigned* i1,unsigned* i2) {
    llvm::GenericValue* res = (llvm::GenericValue*)val;
    *i1 = res->UIntPairVal.first;
    *i2 = res->UIntPairVal.second;
  }

  size_t sizeof_StringRef = sizeof(llvm::StringRef);
  size_t alignof_StringRef = __alignof__(llvm::StringRef);

  void move_StringRef(void* trg,const char* str) {
    *((llvm::StringRef*)trg) = llvm::StringRef(str);
  }

#if HS_LLVM_VERSION>=303
  unsigned llvm_AttributeSet_ReturnIndex = llvm::AttributeSet::ReturnIndex;
  unsigned llvm_AttributeSet_FunctionIndex = llvm::AttributeSet::FunctionIndex;
#endif
  
}
