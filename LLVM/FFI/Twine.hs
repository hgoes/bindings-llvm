module LLVM.FFI.Twine
       (Twine(),
        ToTwine(..),
        deleteTwine
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.CPP.String
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc

class ToTwine a where
  newTwine :: a -> IO (Ptr Twine)
  withTwine :: a -> (Ptr Twine -> IO b) -> IO b
  withTwine x f = do
    tw <- newTwine x
    res <- f tw
    deleteTwine tw
    return res

instance ToTwine (Ptr CChar) where
  newTwine = newTwineCString

instance ToTwine (Ptr CPPString) where
  newTwine = newTwineCPPString

instance ToTwine CChar where
  newTwine = newTwineChar

instance ToTwine CUChar where
  newTwine = newTwineUChar

instance ToTwine CSChar where
  newTwine = newTwineSChar

instance ToTwine CUInt where
  newTwine = newTwineUnsigned

instance ToTwine CInt where
  newTwine = newTwineInt

instance ToTwine (Ptr CLong) where
  newTwine = newTwineLong

instance ToTwine (Ptr CULong) where
  newTwine = newTwineULong

instance ToTwine (Ptr CLLong) where
  newTwine = newTwineLongLong

instance ToTwine (Ptr CULLong) where
  newTwine = newTwineULongLong

instance ToTwine String where
  newTwine str = do
    cstr <- newCString str
    newTwine cstr
  withTwine str f = do
    cstr <- newCString str
    res <- withTwine cstr f
    free cstr
    return res

instance (ToTwine a,ToTwine b) => ToTwine (a,b) where
  newTwine (x,y) = do
    tx <- newTwine x
    ty <- newTwine y
    twineConcat tx ty
  withTwine (x,y) f
    = withTwine x $
      \tx -> withTwine y $
             \ty -> do
               tz <- twineConcat tx ty
               res <- f tz
               deleteTwine tz
               return res
