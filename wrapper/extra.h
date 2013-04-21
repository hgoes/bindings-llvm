#ifndef __HS_LLVM_EXTRA_H__
#define __HS_LLVM_EXTRA_H__

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
