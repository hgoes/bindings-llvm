#ifndef __HS_LLVM_EXTRA_H__
#define __HS_LLVM_EXTRA_H__

#include <stddef.h>
#include <stdint.h>

#if HS_LLVM_VERSION<307
extern char* passId_LoopInfo();
#endif
#if HS_LLVM_VERSION<306
extern const char* passId_FindUsedTypes;
#endif
#if HS_LLVM_VERSION >= 209 && HS_LLVM_VERSION<307
extern char* passId_TargetLibraryInfo();
#endif
#if HS_LLVM_VERSION < 307
#if HS_LLVM_VERSION < 302
extern char* passId_TargetData();
#elif HS_LLVM_VERSION < 305
extern char* passId_DataLayout();
#else
extern char* passId_DataLayoutPass();
#endif
#endif
#if HS_LLVM_VERSION < 305
extern char* passId_DominatorTree();
#else
extern char* passId_DominatorTreeWrapperPass();
#endif
extern char* passId_AliasAnalysis();

extern int writeBitCodeToFile(void* m,const char* path);

extern void* std_string_empty();
extern void* std_string_from_string(char* str);
extern void std_string_delete(void* str);
extern const char* std_string_to_string(void* str);

extern char* value_to_string(void* val);

extern size_t sizeof_APInt;
extern size_t alignof_APInt;

extern size_t sizeof_GenericValue;
extern size_t alignof_GenericValue;

extern void move_APInt(void* trg,unsigned bw,unsigned wcount,uint64_t* arr);
extern void move_APIntSimple(void* trg,unsigned bw,uint64_t val);

extern void genericValueSetIntPair(void* val,unsigned i1,unsigned i2);
extern void genericValueGetIntPair(void* val,unsigned* i1,unsigned* i2);

extern size_t sizeof_StringRef;
extern size_t alignof_StringRef;

void move_StringRef(void* trg,const char* str);

#if HS_LLVM_VERSION>=303
extern unsigned llvm_AttributeSet_ReturnIndex;
extern unsigned llvm_AttributeSet_FunctionIndex;
#endif

#endif
