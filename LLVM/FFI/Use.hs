module LLVM.FFI.Use 
       (Use()
       ,useGet
       ,useGetUser
       ,useGetNext
       ,useToList
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr

useToList :: Ptr Use -> Ptr Use -> IO [Ptr Value]
useToList use end
  | use == end = return []
  | otherwise = do
    val <- useGet use
    use' <- useGetNext use
    vals <- useToList use' end
    return $ val:vals