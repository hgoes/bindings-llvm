#ifndef __HS_LLVM_EXTRA_H__
#define __HS_LLVM_EXTRA_H__

extern char* passId_LoopInfo();
extern char* passId_FindUsedTypes();
#if HS_LLVM_VERSION >= 209
extern char* passId_TargetLibraryInfo();
#endif
#if HS_LLVM_VERSION >= 302
extern char* passId_DataLayout();
#else
extern char* passId_TargetData();
#endif
extern char* passId_DominatorTree();

extern int writeBitCodeToFile(void* m,const char* path);

extern void* std_string_empty();
extern void* std_string_from_string(char* str);
extern void std_string_delete(void* str);
extern const char* std_string_to_string(void* str);

extern char* value_to_string(void* val);
#endif
