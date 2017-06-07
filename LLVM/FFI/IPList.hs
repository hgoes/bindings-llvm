module LLVM.FFI.IPList 
       (Iplist(),
        IPListC(..),
        ipListSize,
        ipListToList,
        ipListToLazyList,
        Ilist_iterator(),
        IListIteratorC(..)
       ) where

import LLVM.FFI.Interface
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafeInterleaveIO)

class IPListC a where
  ipListSize' :: Ptr (Iplist a) -> IO CSize
  ipListBegin :: Ptr (Iplist a) -> IO (Ptr (Ilist_iterator a))
  ipListEnd :: Ptr (Iplist a) -> IO (Ptr (Ilist_iterator a))
  ipListInsert :: Ptr (Iplist a) -> Ptr (Ilist_iterator a) -> Ptr a -> IO (Ptr (Ilist_iterator a))
  ipListRemove :: Ptr (Iplist a) -> Ptr (Ilist_iterator a) -> IO (Ptr a)
  ipListPushFront :: Ptr (Iplist a) -> Ptr a -> IO ()
  ipListPushBack :: Ptr (Iplist a) -> Ptr a -> IO ()

ipListSize :: IPListC a => Ptr (Iplist a) -> IO Integer
ipListSize ptr = fmap fromIntegral $ ipListSize' ptr

class IListIteratorC a where
  iListIteratorDeref :: Ptr (Ilist_iterator a) -> IO (Ptr a)
  iListIteratorNext :: Ptr (Ilist_iterator a)
                       -> IO (Ptr (Ilist_iterator a))
  iListIteratorPrev :: Ptr (Ilist_iterator a)
                    -> IO (Ptr (Ilist_iterator a))
  iListIteratorEq :: Ptr (Ilist_iterator a)
                     -> Ptr (Ilist_iterator a)
                     -> IO Bool
  iListIteratorNEq :: Ptr (Ilist_iterator a)
                      -> Ptr (Ilist_iterator a)
                      -> IO Bool

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

ipListToLazyList :: (IPListC a,IListIteratorC a) 
                => Ptr (Iplist a) -> IO [Ptr a]
ipListToLazyList list = do
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
                 xs <- unsafeInterleaveIO $ toList' nxt end
                 return (x:xs))
