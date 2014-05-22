module LLVM.FFI.OStream 
    (OStreamC(),
     Raw_ostream(),
     deleteOStream,
     Raw_fd_ostream(),
     newFDOStream,
     Raw_string_ostream(),
     newStringOStream
    ) where

import LLVM.FFI.Interface
import System.Posix.Types
import Foreign.Ptr

class OStreamC t

instance OStreamC Raw_ostream
instance OStreamC Raw_fd_ostream
instance OStreamC Raw_string_ostream

deleteOStream :: OStreamC t => Ptr t -> IO ()
deleteOStream = deleteOStream_

newFDOStream :: Fd -> Bool -> Bool -> IO (Ptr Raw_fd_ostream)
newFDOStream (Fd i) shouldClose unbuffered = newFDOStream_ i shouldClose unbuffered
