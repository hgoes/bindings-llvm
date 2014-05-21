module LLVM.FFI.IPList 
       (Iplist(),
        IPListC(..),
        ipListSize,
        ipListToList,
        Ilist_iterator(),
        IListIteratorC(..),
        IListNodeC(..)
       ) where

import LLVM.FFI.Interface
import Foreign
import Foreign.C

class IPListC a where
  ipListSize' :: Ptr (Iplist a) -> IO CSize
  ipListBegin :: Ptr (Iplist a) -> IO (Ptr (Ilist_iterator a))
  ipListEnd :: Ptr (Iplist a) -> IO (Ptr (Ilist_iterator a))
  ipListInsert :: Ptr (Iplist a) -> Ptr (Ilist_iterator a) -> Ptr a -> IO (Ptr (Ilist_iterator a))
  ipListRemove :: Ptr (Iplist a) -> Ptr (Ilist_iterator a) -> IO (Ptr a)

ipListSize :: IPListC a => Ptr (Iplist a) -> IO Integer
ipListSize ptr = fmap fromIntegral $ ipListSize' ptr

class IListIteratorC a where
  iListIteratorDeref :: Ptr (Ilist_iterator a) -> IO (Ptr a)
  iListIteratorNext :: Ptr (Ilist_iterator a)
                       -> IO (Ptr (Ilist_iterator a))
  iListIteratorEq :: Ptr (Ilist_iterator a)
                     -> Ptr (Ilist_iterator a)
                     -> IO Bool
  iListIteratorNEq :: Ptr (Ilist_iterator a)
                      -> Ptr (Ilist_iterator a)
                      -> IO Bool

class IListNodeC a where
  iListNodeNext :: Ptr a -> IO (Ptr a)
  iListNodePrev :: Ptr a -> IO (Ptr a)

ipListToList :: (IPListC a,IListIteratorC a) 
                => Ptr (Iplist a) -> IO [Ptr a]
ipListToList list = do
  begin <- ipListBegin list
  end <- ipListEnd list
  toList' begin end
  where
    toList' cur end = do
      isEnd <- iListIteratorEq cur end
      if isEnd
        then return []
        else (do
                 x <- iListIteratorDeref cur
                 nxt <- iListIteratorNext cur
                 xs <- toList' nxt end
                 return (x:xs))
