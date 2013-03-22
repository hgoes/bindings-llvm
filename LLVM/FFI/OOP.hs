module LLVM.FFI.OOP where

import Foreign
import Data.Proxy

class Subtype super sub where
  isA :: Proxy sub -> Ptr super -> Bool

castUp :: Subtype super sub => Ptr sub -> Ptr super
castUp = castPtr

castDown :: Subtype super sub => Ptr super -> Maybe (Ptr sub)
castDown ptr = withProxy $ \pr -> if isA pr ptr
                                  then Just (castPtr ptr)
                                  else Nothing
  where
    withProxy :: (Proxy sub -> Maybe (Ptr sub)) -> Maybe (Ptr sub)
    withProxy f = f Proxy

{-
data ClassHierarchy
  = CH String [(ClassHierarchy,String)]

mkHierarchy :: ClassHierarchy -> Q [Dec]
mkHierarchy = mkHierarchy' [] Nothing
  where 
    mkHierarchy' stack fun (CH node subs) = do
      let node_name = mkName node
          cls_name = mkName (node++"C")
      decl_node <- dataD (cxt []) node_name [] [normalC node_name []] []
      arg_t <- newName "t"
      (cls_nodes,stack') <- case subs of
        [] -> return ([],stack)
        _ -> do
          cls_node <- classD (cxt []) cls_name [PlainTV arg_t] [] []
          return $ ([cls_node],(node_name,cls_name):stack)
      insts1 <- mapM (\(_,cls) -> instanceD (cxt []) (appT (conT cls) (conT node_name)) []) stack'
      insts2 <- case fun of
        Nothing -> return []
        Just fname -> mapM (\(nd,_) 
                            -> instanceD (cxt []) ((conT ''Subtype) `appT` (conT nd) `appT` (conT node_name))
                               [funD 'isA [clause [wildP] (normalB $ varE $ mkName fname) []] ]
                                ) stack'
      insts3 <- case fun of
        Nothing -> return []
        Just fname -> do
           t <- newName "t"
           return [ForeignD (ImportF CCall Unsafe 
                              ("llvm_proxy.h "++fname)
                              (mkName fname)
                              (ForallT [(PlainTV t)] [] $
                                ArrowT `AppT` (AppT (ConT ''Ptr) (VarT t)) `AppT` (ConT ''Bool)))]
      rests <- mapM (\(sub,fname) -> mkHierarchy' stack' (Just fname) sub) subs
      return $ [decl_node]++cls_nodes++insts1++insts2++insts3++concat rests -}