module LLVM.FFI.Pass
       (Pass()
       ,PassC()
       ,deletePass
       ,passLookupPassInfo
       ,FunctionPass()
       ,ModulePass()
       ,ModulePassC()
       ,modulePassRunOnModule
       ,ImmutablePass()
       ,ImmutablePassC()
       ,FindUsedTypes()
       ,newFindUsedTypes
       ,deleteFindUsedTypes
       ,findUsedTypesGetTypes
       ,TargetLibraryInfo()
       ,LibFunc(..)
       ,newTargetLibraryInfo
       ,deleteTargetLibraryInfo
       ,targetLibraryInfoGetLibFunc
       ,targetLibraryInfoGetName
       ,targetLibraryInfoHas
       ,DataLayout()
       ,newDataLayoutFromString
       ,newDataLayoutFromModule
       ,dataLayoutIsLittleEndian
       ,dataLayoutIsBigEndian
       ,createCFGSimplificationPass
       ) where

import LLVM.FFI.Interface
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

#include "Helper.h"

class PassC t

instance PassC Pass
instance PassC FunctionPass
instance PassC ModulePass
instance PassC ImmutablePass
instance PassC FindUsedTypes
instance PassC TargetLibraryInfo
instance PassC DataLayout

class ModulePassC t

instance ModulePassC ModulePass
instance ModulePassC ImmutablePass
instance ModulePassC FindUsedTypes
instance ModulePassC TargetLibraryInfo
instance ModulePassC DataLayout

class ImmutablePassC t
instance ImmutablePassC TargetLibraryInfo
instance ImmutablePassC DataLayout

deletePass :: PassC t => Ptr t -> IO ()
deletePass = deletePass_

modulePassRunOnModule :: ModulePassC p => Ptr p -> Ptr Module -> IO Bool
modulePassRunOnModule = modulePassRunOnModule_

targetLibraryInfoGetLibFunc :: Ptr TargetLibraryInfo -> Ptr StringRef -> IO (Maybe LibFunc)
targetLibraryInfoGetLibFunc tli str
  = alloca (\iptr -> do
               res <- targetLibraryInfoGetLibFunc_ tli str iptr
               if res
                 then (do
                          ires <- peek iptr
                          return (Just $ toLibFunc ires))
                 else return Nothing)

targetLibraryInfoGetName :: Ptr TargetLibraryInfo -> LibFunc -> IO (Ptr StringRef)
targetLibraryInfoGetName ptr f = targetLibraryInfoGetName_ ptr (fromLibFunc f)

targetLibraryInfoHas :: Ptr TargetLibraryInfo -> LibFunc -> IO Bool
targetLibraryInfoHas ptr f = targetLibraryInfoHas_ ptr (fromLibFunc f)

data LibFunc =
#define HANDLE_LIBFUNC(name) PRESERVE(  ) Func_##name
#define HANDLE_SEP PRESERVE(  ) |
#include "LibFunc.def"
  deriving (Show,Eq,Ord)

toLibFunc :: CInt -> LibFunc
toLibFunc op
#define HANDLE_LIBFUNC(name) PRESERVE(  ) | op == libFunc_##name = Func_##name
#include "LibFunc.def"

fromLibFunc :: LibFunc -> CInt
#define HANDLE_LIBFUNC(name) fromLibFunc Func_##name = libFunc_##name
#include "LibFunc.def"

#define HANDLE_LIBFUNC(name) foreign import capi _TO_STRING(extra.h LibFunc_##name) libFunc_##name :: CInt
#include "LibFunc.def"