module LLVM.FFI.Use 
       (Use()
       ,useGet
       ,useSet
       ,useGetUser
       ,useGetNext
       ,useToList
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Value (ValueC)
import Foreign.Ptr

useSet :: ValueC v => Ptr Use -> Ptr v -> IO ()
useSet = useSet_

useToList :: Ptr Use -> Ptr Use -> IO [Ptr Value]
useToList use end
  | use == end || use == nullPtr = return []
  | otherwise = do
    val <- useGet use
    use' <- useGetNext use
    vals <- useToList use' end
    return $ val:vals
