module LLVM.FFI.Twine
       (Twine(),
        newTwineEmpty,
        newTwineString
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr
import Foreign.C.String

newTwineString :: String -> IO (Ptr Twine)
newTwineString str = do
  cstr <- newCString str
  newTwineString_ cstr