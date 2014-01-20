module Generator where

import CPPType
import Data.List
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Data.Char
import System.FilePath
import Data.Ord

data Spec
  = Spec { specHeader :: String
         , specNS :: NS
         , specName :: String
         , specTemplateArgs :: [Type]
         , specType :: SpecType
         }

data SpecType
  = ClassSpec { cspecFuns :: [(FunSpec,String)]
              , isInterface :: Bool }
  | GlobalFunSpec { gfunReturnType :: Type
                  , gfunArgs :: [(Bool,Type)]
                  , gfunHSName :: String
                  }
  | EnumSpec { enumHSName :: String
             , enumElems :: [(String,String)]
             }

classSpec :: [(FunSpec,String)] -> SpecType
classSpec funs = ClassSpec funs False

interfaceSpec :: [(FunSpec,String)] -> SpecType
interfaceSpec funs = ClassSpec funs True

specFullName :: Spec -> String
specFullName cs = renderNS (specNS cs) ++
                  specName cs ++
                  renderTempl (specTemplateArgs cs)

specFullNameHS :: Spec -> String
specFullNameHS cs = (concat [ cname++"_" | ClassName cname _ <- specNS cs ])++
                    (specName cs)

specFullType :: Spec -> Type
specFullType cs = Type [] (NamedType (specNS cs) (specName cs) (specTemplateArgs cs)
                           (case specType cs of
                               ClassSpec { isInterface = iface } -> iface
                               _ -> False))

data FunSpec = Constructor { ftConArgs :: [(Bool,Type)]
                           }
             | Destructor { ftOverloadedDestructor :: Bool }
             | MemberFun { ftReturnType :: Type
                         , ftName :: String
                         , ftTemplArgs :: [Type]
                         , ftArgs :: [(Bool,Type)]
                         , ftStatic :: Bool
                         , ftOverloaded :: Bool
                         , ftPure :: Bool
                         , ftIgnoreReturn :: Bool
                         }
             | Setter { ftSetVar :: String
                      , ftSetType :: Type
                      }
             | Getter { ftGetVar :: String
                      , ftGetType :: Type
                      , ftGetStatic :: Bool
                      }
             | SizeOf

type OutConverter = String -> ([String],String)
type InConverter = String -> ([String],String)

memberFun :: FunSpec
memberFun = MemberFun { ftReturnType = normalT void
                      , ftName = ""
                      , ftTemplArgs = []
                      , ftArgs = []
                      , ftStatic = False
                      , ftOverloaded = False
                      , ftPure = False
                      , ftIgnoreReturn = False
                      }

idOut :: OutConverter
idOut x = ([],x)

idIn :: InConverter
idIn x = ([],x)

enumCastIn :: NS -> String -> InConverter
enumCastIn ns name x = ([],"static_cast<"++renderType (normalT $ EnumType ns name)++">("++x++")")

copyOut :: Type -> OutConverter
copyOut tp x = ([],"new "++renderType tp++"("++x++")")

addressOut :: OutConverter
addressOut x = ([],"&("++x++")")

passAsPointer :: Type -> InConverter
passAsPointer tp x = ([],"*(("++renderType tp++"*)"++x++")")

refToPtr :: Type -> OutConverter
refToPtr tp x = ([],"("++(renderType tp)++"*) &"++x)

ptrToRef :: Type -> InConverter
ptrToRef tp x = ([],"*("++x++")")

voidCastOut :: Bool -> OutConverter
voidCastOut isConst x = ([],"("++(if isConst
                                  then "const "
                                  else "")++"void*)("++x++")")

voidCastIn :: Type -> InConverter
voidCastIn tp x = ([],"("++renderType tp++"*)("++x++")")

renderType :: Type -> String
renderType (Type qual tp) 
  = concat (fmap (\q -> renderQualifier q++" ") qual)
    ++ renderC tp
  where
    renderQualifier QConst = "const"
    renderC (NamedType ns str templ _) = renderNS ns ++ str ++ renderTempl templ
    renderC (EnumType ns str) = renderNS ns ++ str
    renderC (PtrType tp) = renderC tp++"*"
    renderC (RefType tp) = renderC tp++"&"
renderType (TypeInt n) = show n

renderNS :: NS -> String
renderNS = concat . fmap (\ns -> className ns++renderTempl (classArgs ns)++"::")

renderTempl :: [Type] -> String
renderTempl [] = ""
renderTempl xs = "<"++concat (intersperse "," $ fmap renderType xs)++" >"

normalT :: TypeC -> Type
normalT c = Type [] c

constT :: TypeC -> Type
constT = Type [QConst]

char = NamedType [] "char" [] False
void = NamedType [] "void" [] False
size_t = NamedType [] "size_t" [] False
bool = NamedType [] "bool" [] False
unsigned = NamedType [] "unsigned" [] False
int = NamedType [] "int" [] False
uint64_t = NamedType [] "uint64_t" [] False
uint8_t = NamedType [] "uint8_t" [] False
int64_t = NamedType [] "int64_t" [] False
double = NamedType [] "double" [] False
float = NamedType [] "float" [] False
ptr = PtrType
ref = RefType
llvmType name = NamedType llvmNS name [] False

llvmNS = [ClassName "llvm" []]

toPtr :: Type -> Type
toPtr (Type qual tp) = Type qual (ptr tp)

toConstRef :: Type -> Type
toConstRef (Type _ tp) = Type [QConst] (RefType tp)

toConstPtr :: Type -> Type
toConstPtr (Type _ tp) = Type [QConst] (PtrType tp)


cstring = PtrType char

isCType :: TypeC -> Bool
isCType (NamedType [] name [] _) = case name of
  "void" -> True
  "char" -> True
  "size_t" -> True
  "int" -> True
  "int64_t" -> True
  "uint64_t" -> True
  "uint8_t" -> True
  "bool" -> True
  "unsigned" -> True
  "double" -> True
  "float" -> True
  _ -> False
isCType (PtrType tp) = isCType tp
isCType _ = False

toCType :: Type -> (Type,OutConverter,InConverter)
toCType (Type q c) = let (x,out,inC) = toCType' c
                     in (Type q x,out,inC)
  where
    toCType' (RefType t) = let (tp',outC',inC') = toCType' (PtrType t)
                           in (tp',\x -> let (out1,r1) = refToPtr (Type q t) x
                                             (out2,r2) = outC' r1
                                         in (out1++out2,r2),
                               \x -> let (out1,r1) = inC' x
                                         (out2,r2) = ptrToRef (Type q t) r1 
                                     in (out1++out2,r2))
    toCType' (PtrType t) = if isCType t
                           then (PtrType t,idOut,idIn)
                           else (PtrType void,voidCastOut (QConst `elem` q),voidCastIn (Type q t))
    toCType' (EnumType ns name) = (NamedType [] "int" [] False,idOut,enumCastIn ns name)
    toCType' t = if isCType t
                 then (t,idOut,idIn)
                 else (ptr void,copyOut (Type q t),passAsPointer (Type q t))

toHaskellType :: Bool -> Maybe String -> Type -> HsType
toHaskellType _ (Just v) _
  = HsTyApp (HsTyCon $ UnQual $ HsIdent "Ptr") (HsTyVar $ HsIdent v)
toHaskellType addP Nothing (Type q c) = toHSType (not addP) c
  where
    toHSType _ (RefType t) = HsTyApp 
                             (HsTyCon $ UnQual $ HsIdent "Ptr")
                             (toHSType True t)
    toHSType _ (PtrType t) = HsTyApp 
                             (HsTyCon $ UnQual $ HsIdent "Ptr")
                             (toHSType True t)
    toHSType isP (NamedType [] name [] iface) = case name of
      "void" -> HsTyTuple []
      "char" -> HsTyCon $ UnQual $ HsIdent "CChar"
      "size_t" -> HsTyCon $ UnQual $ HsIdent "CSize"
      "int" -> HsTyCon $ UnQual $ HsIdent "CInt"
      "int64_t" -> HsTyCon $ UnQual $ HsIdent "Int64"
      "uint64_t" -> HsTyCon $ UnQual $ HsIdent "Word64"
      "uint8_t" -> HsTyCon $ UnQual $ HsIdent "Word8"
      "bool" -> HsTyCon $ UnQual $ HsIdent "Bool"
      "unsigned" -> HsTyCon $ UnQual $ HsIdent "CUInt"
      "double" -> HsTyCon $ UnQual $ HsIdent "CDouble"
      "float" -> HsTyCon $ UnQual $ HsIdent "CFloat"
      _ -> (if isP
            then id 
            else HsTyApp (HsTyCon $ UnQual $ HsIdent "Ptr")
           ) $ HsTyCon $ UnQual $ HsIdent $ if iface
                                            then fmap toLower name
                                            else hsName name
    toHSType isP (NamedType ns name tmpl iface) 
      = (if isP
         then id
         else HsTyApp (HsTyCon $ UnQual $ HsIdent "Ptr")
        ) $ foldl HsTyApp (toHSType True (NamedType [] name [] iface))
        (if iface
         then []
         else (fmap (toHaskellType False Nothing) $
               filter (\tp -> case tp of
                          Type _ _ -> True
                          _ -> False) $
               concat (fmap classArgs ns)++tmpl))
    toHSType isP (EnumType ns name)
      = HsTyCon $ UnQual $ HsIdent "CInt"

writeWrapper :: String -> [Spec] -> String -> String -> String -> [String] -> IO ()
writeWrapper inc_sym spec build_path header_f wrapper_f ffi_f = do
  let (hcont,wcont) = generateWrapper inc_sym spec
  writeFile (build_path </> header_f) hcont
  writeFile (build_path </> wrapper_f) wcont
  writeFile (build_path </> joinPath ffi_f <.> "hs") 
    (generateFFI ffi_f header_f spec)

generateWrapper :: String -> [Spec] -> (String,String)
generateWrapper inc_sym spec
  = let includes = ["#include <"++cs++">" | cs <- nub $ fmap specHeader spec]
        all_cont = concat [ case specType cs of
                               ClassSpec funs _ -> fmap (generateWrapperFunction cs) funs
                               GlobalFunSpec rtp args hsname -> [generateGlobalWrapper cs rtp args hsname]
                               EnumSpec _ elems -> [generateEnum cs elems]
                          | cs <- spec ]
        header_cont = unlines $ ["#ifndef "++inc_sym
                                ,"#define "++inc_sym
                                ,"#include <stdint.h>"
                                ,"#include <stdlib.h>"
                                ,"#include <stdbool.h>"] ++
                      concat (fmap fst all_cont) ++ ["#endif"]
        wrapper_cont = unlines $ includes ++ ["extern \"C\" {"] ++ 
                       concat (fmap snd all_cont) ++ ["}"]
    in (header_cont,wrapper_cont)
  where
    generateWrapperFunction' :: Type -> String -> [(Type,String)] -> ([(Type,String)] -> ([String],String)) -> Bool -> ([String],[String])
    generateWrapperFunction' rtp name args body ignore
      = let sig = renderType rtp'++" "++name++
                  "("++(paramList $ fmap (\(tp,n,_) -> (tp,n)) args')++")"
            (rtp',outC,_) = toCType rtp
            (args',cmds) = unzip $ fmap (\(tp,name) -> let (tp',_,inC) = toCType tp
                                                           (cmds,res) = inC name
                                                       in ((tp',name,res),cmds)
                                        ) args
            (act,res1) = body $ fmap (\(tp,_,n) -> (tp,n)) args'
            (conv,res2) = outC res1
        in ([sig++";"],[sig++" {"]++
                       concat cmds++
                       act++
                       conv++
                       [if ignore
                        then "  "++res2++";"
                        else "  return "++res2++";"
                       ,"}"])
    
    generateGlobalWrapper :: Spec -> Type -> [(Bool,Type)] -> String -> ([String],[String])
    generateGlobalWrapper cls rtp args hsname
      = generateWrapperFunction' rtp hsname (mkArgs (fmap snd args))
        (\args' -> ([],specFullName cls++"("++argList args'++")")) False
    
    generateWrapperFunction :: Spec -> (FunSpec,String) -> ([String],[String])
    generateWrapperFunction cls (fun,as)
      = let args = case fun of
              Constructor args -> mkArgs $ fmap snd args
              Destructor _ -> [(self_ptr,"self")]
              MemberFun { ftArgs = args 
                        , ftStatic = stat } -> (if stat
                                                then id
                                                else ((self_ptr,"self"):)) (mkArgs $ fmap snd $ args)
              Setter { ftSetVar = var
                     , ftSetType = tp } -> [(self_ptr,"self"),(tp,"value")]
              Getter { ftGetStatic = stat } -> if stat
                                               then []
                                               else [(self_ptr,"self")]
              SizeOf -> []
            self_ptr = toPtr (specFullType cls)
            rt = case fun of
              Constructor _ -> normalT $ ptr void
              Destructor _ -> normalT void
              MemberFun { ftReturnType = tp } -> tp
              Setter {} -> normalT void
              Getter { ftGetType = tp } 
                -> case tp of
                Type _ tp' -> if isCType tp'
                              then tp
                              else toPtr tp
              SizeOf -> normalT size_t
            body = case fun of
              Constructor _ -> \args' -> ([],"new "++specFullName cls++"("++argList args'++")")
              Destructor _ -> \[(_,n)] -> (["delete "++n++";"],"")
              MemberFun { ftName = name 
                        , ftStatic = stat 
                        , ftTemplArgs = tmpl } 
                -> \args' -> let rself = snd $ head args'
                                 rargs = if stat
                                         then args'
                                         else tail args'
                                 targs = case tmpl of
                                   [] -> ""
                                   _ -> "<"++concat (intersperse "," (fmap renderType tmpl))++">"
                                 call = (if stat
                                         then specFullName cls ++ "::"
                                         else "("++rself++")->")++name++targs++
                                              "("++argList rargs++")"
                             in ([],call)
              Getter { ftGetVar = name
                     , ftGetType = tp
                     , ftGetStatic = stat
                     } -> if stat
                          then \_ -> ([],specFullName cls++"::"++name)
                          else \[(_,self)] 
                               -> case tp of
                                 Type _ tp' -> if isCType tp'
                                               then ([],"("++self++")->"++name)
                                               else ([],"&(("++self++")->"++name++")")
              Setter { ftSetVar = name
                     , ftSetType = tp
                     } -> \[(_,self),(_,val)] -> (["("++self++")->"++name++" = "++val++";"],"")
              SizeOf -> \[] -> ([],"sizeof("++specFullName cls++")")
            ignore = case fun of
              MemberFun { ftIgnoreReturn = i } -> i
              _ -> False
        in generateWrapperFunction' rt as args body ignore
    generateEnum :: Spec -> [(String,String)] -> ([String],[String])
    generateEnum cls elems
      = (["extern int enum_"++specFullNameHS cls++"_"++el++"();"
         | (el,_) <- elems ],
         ["int enum_"++specFullNameHS cls++"_"++el++"() { return "++renderNS (specNS cls)++el++"; }"
         | (el,_) <- elems ])

generateFFI :: [String] -> String -> [Spec] -> String
generateFFI mname header specs 
  = unlines (["module "++concat (intersperse "." mname)++" where"
             ,""
             ,"import Foreign"
             ,"import Foreign.C"
             ,""]++dts++fns++conv)
  where
    dts = [ "data "++hsName (specName cs) ++
            concat (fmap (\(_,i) -> " a"++show i) (zip (specCollectTemplateArgs cs) [0..]))++
            " = "++hsName (specName cs)
          | cs@Spec { specType = ClassSpec { isInterface = False } } <- nubBy (\x y -> specName x == specName y) specs
          ] ++
          [ "data "++hsname++" = "++(concat $ intersperse " | " [ el | (_,el) <- els ])++" deriving (Show,Eq,Ord)"
          | Spec { specType = EnumSpec { enumHSName = hsname
                                       , enumElems = els } } <- specs ]
    conv = concat
           [ ["to"++hsname++" :: CInt -> "++hsname
             ,"to"++hsname++" op"
             ]++[ "  | op == enum_"++cname++"_"++el++" = "++el_hs
                | (el,el_hs) <- els ]++
             ["","from"++hsname++" :: "++hsname++" -> CInt"]++
             ["from"++hsname++" "++el_hs++" = enum_"++cname++"_"++el
             | (el,el_hs) <- els ]
           | cs@Spec { specType = EnumSpec { enumHSName = hsname
                                           , enumElems = els }
                     } <- specs
           , let cname = specFullNameHS cs
           ]
    fns = concat [ [""
                   ,"foreign import capi \""++header++" "++c_name++"\""
                   ,"  "++c_name++" :: "++sig]
                 | cs <- specs
                 , (tps,rtp,c_name) <- case specType cs of
                   ClassSpec { cspecFuns = funs } 
                     -> fmap (\(fun,cname)
                              -> case fun of
                                MemberFun { ftArgs = r
                                          , ftOverloaded = isO 
                                          , ftPure = isP 
                                          , ftStatic = isS
                                          , ftReturnType = rtp }
                                  -> ((if isS
                                       then id
                                       else ((toHaskellType True (if isO
                                                                  then Just "t"
                                                                  else Nothing) $ toPtr $ specFullType cs):))
                                      (fmap (\((isO',tp'),n) -> toHaskellType True (if isO'
                                                                                    then Just $ "t"++show n
                                                                                    else Nothing) tp') (zip r [0..])),
                                      (if isP
                                       then id
                                       else HsTyApp (HsTyCon $ UnQual $ HsIdent "IO"))
                                      (toHaskellType True Nothing rtp),
                                      cname)
                                Constructor { ftConArgs = r }
                                  -> (fmap (\((isO',tp'),n) -> toHaskellType True (if isO'
                                                                                   then Just $ "t"++show n
                                                                                   else Nothing) tp') (zip r [0..]),
                                      HsTyApp (HsTyCon $ UnQual $ HsIdent "IO") $
                                      toHaskellType True Nothing $ toPtr $ specFullType cs,
                                      cname)
                                Destructor { ftOverloadedDestructor = isO }
                                  -> ([toHaskellType True (if isO
                                                           then Just "t"
                                                           else Nothing) $ toPtr $ specFullType cs],
                                      HsTyApp (HsTyCon $ UnQual $ HsIdent "IO") $
                                      toHaskellType True Nothing $ normalT void,
                                      cname)
                                Setter { ftSetType = tp }
                                  -> ([toHaskellType True Nothing $ toPtr $ specFullType cs,
                                       toHaskellType True Nothing tp],
                                      HsTyApp (HsTyCon $ UnQual $ HsIdent "IO") $
                                      toHaskellType True Nothing $ normalT void,
                                      cname)
                                Getter { ftGetType = tp
                                       , ftGetStatic = stat }
                                  -> (if stat
                                      then []
                                      else [toHaskellType True Nothing $ toPtr $ specFullType cs],
                                      HsTyApp (HsTyCon $ UnQual $ HsIdent "IO") $
                                      toHaskellType True Nothing tp,
                                      cname)
                                SizeOf -> ([],toHaskellType True Nothing (normalT size_t),cname)
                             ) funs
                   GlobalFunSpec { gfunReturnType = rtp
                                 , gfunArgs = args
                                 , gfunHSName = hsname
                                 } -> [(fmap (\((isO',tp'),n) -> toHaskellType True (if isO'
                                                                                     then Just $ "t"++show n
                                                                                     else Nothing) tp') (zip args [0..]),
                                        HsTyApp (HsTyCon $ UnQual $ HsIdent "IO") $
                                        toHaskellType True Nothing rtp,
                                        hsname)]
                   EnumSpec { enumElems = els
                            } -> [([],toHaskellType True Nothing $ normalT int,"enum_"++specFullNameHS cs++"_"++el)
                                 | (el,_) <- els ]
                 , let sig = (concat [ prettyPrint tp ++ " -> " 
                                     | tp <- tps
                                     ]) ++
                             prettyPrint rtp
                 ]

hsName :: String -> String
hsName (c:cs) = if isLower c
                then toUpper c:cs
                else c:cs

mkArgs :: [Type] -> [(Type,String)]
mkArgs tps = fmap (\(tp,i) -> (tp,"arg"++show i)) (zip tps [0..])

paramList :: [(Type,String)] -> String
paramList = comma . fmap (\(tp,name) -> renderType tp++" "++name)

comma :: [String] -> String
comma = concat . intersperse ","

argList :: [(Type,String)] -> String
argList = comma . fmap (\(_,arg) -> arg)

specCollectTemplateArgs :: Spec -> [Type]
specCollectTemplateArgs spec 
  = filter (\tp -> case tp of
               Type _ _ -> True
               _ -> False) $
    concat (fmap classArgs (specNS spec)) ++
    specTemplateArgs spec
