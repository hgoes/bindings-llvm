#ifndef __HS_LLVM_EXTRA_H__
#define __HS_LLVM_EXTRA_H__

#define HANDLE_FPRED(name) extern int FCMP_##name();
#define HANDLE_IPRED(name) extern int ICMP_##name();
#include "Predicate.def"

#define HANDLE_CC(name) extern int CConv_##name();
#include "CConvs.def"

#define HANDLE_ORDERING(name) extern int AtomicOrdering_##name();
#include "AtomicOrdering.def"

#define HANDLE_BINOP(name) extern int RMWBinOp_##name();
#include "RMWBinOp.def"

#define HANDLE_LIBFUNC(name) extern int LibFunc_##name();
#include "LibFunc.def"

#define HANDLE_ALIAS_RESULT(name) extern int AliasResult_##name();
#include "Alias.def"

#define HANDLE_SYNC_SCOPE(name) extern int SynchronizationScope_##name();
#include "SyncScope.def"

extern char* passId_LoopInfo();
extern char* passId_FindUsedTypes();
extern char* passId_TargetLibraryInfo();
#if HS_LLVM_VERSION >= 303
extern char* passId_DataLayout();
#else
extern char* passId_TargetData();
#endif


extern int writeBitCodeToFile(void* m,const char* path);

#endif
