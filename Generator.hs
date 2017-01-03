module Generator where

import CPPType
import Data.List
import Data.Char
import System.FilePath
import System.Directory
import Data.Time.Clock
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
              , isInterface :: Bool
              , customDecl :: Maybe String }
  | GlobalFunSpec { gfunReturnType :: Type
                  , gfunArgs :: [(Bool,Type)]
                  , gfunHSName :: String
                  }
  | EnumSpec EnumNode

data EnumNode = EnumNode { enumNodeHSName :: String
                         , enumNodeSubs :: [Either (String,EnumNode) EnumLeaf] }

data EnumLeaf = EnumLeaf { enumLeafCName :: String
                         , enumLeafHSName :: String }

allCEnums :: EnumNode -> [String]
allCEnums nd = concat [ allCEnums' el
                      | el <- enumNodeSubs nd ]
  where
    allCEnums' (Left (_,nd)) = allCEnums nd
    allCEnums' (Right leaf) = [enumLeafCName leaf]

allHSEnums :: EnumNode -> [[String]]
allHSEnums nd = concat [ allHSEnums' el
                       | el <- enumNodeSubs nd ]
  where
    allHSEnums' (Left (name,sub)) = fmap (name:) $ allHSEnums sub
    allHSEnums' (Right leaf) = [[enumLeafHSName leaf]]

classSpec :: [(FunSpec,String)] -> SpecType
classSpec funs = ClassSpec funs False Nothing

interfaceSpec :: [(FunSpec,String)] -> SpecType
interfaceSpec funs = ClassSpec funs True Nothing

classSpecCustom :: String -> [(FunSpec,String)] -> SpecType
classSpecCustom decl funs = ClassSpec funs False (Just decl)

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
                      , ftSetOverloaded :: Bool
                      }
             | Getter { ftGetVar :: String
                      , ftGetType :: Type
                      , ftGetStatic :: Bool
                      }
             | SizeOf
             | AlignOf

type OutConverter = String -> ([String],String)
type InConverter = String -> ([String],String)

bracket :: String -> String
bracket str = case words str of
  [_] -> str
  _ -> "("++str++")"

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

passAsUniquePtr :: Type -> InConverter
passAsUniquePtr tp x = ([],"std::move(*((std::unique_ptr<"++renderType tp++">*)"++x++"))")

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
uchar = NamedType [] "unsigned char" [] False
schar = NamedType [] "signed char" [] False
void = NamedType [] "void" [] False
size_t = NamedType [] "size_t" [] False
bool = NamedType [] "bool" [] False
unsigned = NamedType [] "unsigned" [] False
signed = NamedType [] "signed" [] False
int = NamedType [] "int" [] False
uint64_t = NamedType [] "uint64_t" [] False
uint8_t = NamedType [] "uint8_t" [] False
int64_t = NamedType [] "int64_t" [] False
double = NamedType [] "double" [] False
float = NamedType [] "float" [] False
long = NamedType [] "long" [] False
ulong = NamedType [] "unsigned long" [] False
longlong = NamedType [] "long long" [] False
ulonglong = NamedType [] "unsigned long long" [] False
ptr = PtrType
ref = RefType

toPtr :: Type -> Type
toPtr (Type qual tp) = Type qual (ptr tp)

toConstRef :: Type -> Type
toConstRef (Type _ tp) = Type [QConst] (RefType tp)

toConstPtr :: Type -> Type
toConstPtr (Type _ tp) = Type [QConst] (PtrType tp)

toRef :: Type -> Type
toRef (Type qual tp) = Type qual (ref tp)

cstring = PtrType char

isCType :: TypeC -> Bool
isCType (NamedType [] name [] _) = case name of
  "void" -> True
  "char" -> True
  "unsigned char" -> True
  "signed char" -> True
  "size_t" -> True
  "int" -> True
  "int64_t" -> True
  "uint64_t" -> True
  "uint8_t" -> True
  "bool" -> True
  "unsigned" -> True
  "signed" -> True
  "double" -> True
  "float" -> True
  "long" -> True
  "unsigned long" -> True
  "long long" -> True
  "unsigned long long" -> True
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
    toCType' t@(NamedType [ClassName "std" []] "unique_ptr" [val] False)
      = (PtrType void,copyOut (Type q t),passAsUniquePtr val)
    toCType' t = if isCType t
                 then (t,idOut,idIn)
                 else (ptr void,copyOut (Type q t),passAsPointer (Type q t))

toHaskellType :: Bool -> Maybe String -> Type -> String
toHaskellType _ (Just v) _
  = "Ptr "++v
toHaskellType addP Nothing (Type q c) = toHSType (not addP) c
  where
    toHSType _ (RefType t) = "Ptr "++bracket (toHSType True t)
    toHSType _ (PtrType t) = "Ptr "++bracket (toHSType True t)
    toHSType isP (NamedType [ClassName "std" []] "string" [] False)
      = if isP then "CPPString"
        else "Ptr CPPString"
    toHSType isP (NamedType [ClassName "std" []] "unique_ptr" [arg] False)
      = if isP then "Unique_ptr "++toHaskellType False Nothing arg
        else "Ptr (Unique_ptr "++toHaskellType False Nothing arg++")"
    toHSType isP (NamedType [] name [] iface) = case name of
      "void" -> "()"
      "char" -> "CChar"
      "unsigned char" -> "CUChar"
      "signed char" -> "CSChar"
      "size_t" -> "CSize"
      "int" -> "CInt"
      "int64_t" -> "Int64"
      "uint64_t" -> "Word64"
      "uint8_t" -> "Word8"
      "bool" -> "Bool"
      "unsigned" -> "CUInt"
      "signed" -> "CInt"
      "double" -> "CDouble"
      "float" -> "CFloat"
      "long" -> "CLong"
      "unsigned long" -> "CULong"
      "long long" -> "CLLong"
      "unsigned long long" -> "CULLong"
      _ -> (if isP
            then id 
            else ("Ptr "++).bracket
           ) $ if iface
               then fmap toLower name
               else hsName name
    toHSType isP (NamedType ns name tmpl iface) 
      = (if isP
         then id
         else ("Ptr "++).bracket
        ) $ intercalate " " $ bracket (toHSType True (NamedType [] name [] iface)):
        (if iface
         then []
         else (fmap (bracket . toHaskellType False Nothing) $
               filter (\tp -> case tp of
                          Type _ _ -> True
                          _ -> False) $
               concat (fmap classArgs ns)++tmpl))
    toHSType isP (EnumType ns name)
      = "CInt"

whenOlder :: (FilePath -> IO ()) -> FilePath -> UTCTime -> IO ()
whenOlder act fp modTime = do
  exists <- doesFileExist fp
  if exists
    then (do
             modTime' <- getModificationTime fp
             if modTime' < modTime
               then act fp
               else return ())
    else act fp

writeWrapper :: String -> [Spec] -> String -> UTCTime -> String -> String -> [String] -> IO ()
writeWrapper inc_sym spec build_path modTime header_f wrapper_f ffi_f = do
  let (hcont,wcont) = generateWrapper inc_sym spec
  whenOlder (\path ->writeFile path hcont)
    (build_path </> header_f) modTime
  whenOlder (\path -> writeFile path wcont)
    (build_path </> wrapper_f) modTime
  whenOlder (\path -> writeFile path (generateFFI ffi_f header_f spec))
    (build_path </> joinPath ffi_f <.> "hs") modTime

generateWrapper :: String -> [Spec] -> (String,String)
generateWrapper inc_sym spec
  = let includes = ["#include <"++cs++">" | cs <- nub $ fmap specHeader spec]
        all_cont = concat [ case specType cs of
                               ClassSpec funs _ _ -> fmap (generateWrapperFunction cs) funs
                               GlobalFunSpec rtp args hsname -> [generateGlobalWrapper cs rtp args hsname]
                               EnumSpec nd -> [generateEnum cs (allCEnums nd)]
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
              AlignOf -> []
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
              AlignOf -> normalT size_t
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
              AlignOf -> \[] -> ([],"__alignof__("++specFullName cls++")")
            ignore = case fun of
              MemberFun { ftIgnoreReturn = i } -> i
              _ -> False
        in generateWrapperFunction' rt as args body ignore
    generateEnum :: Spec -> [String] -> ([String],[String])
    generateEnum cls elems
      = (["extern int enum_"++specFullNameHS cls++"_"++el++"();"
         | el <- elems ],
         ["int enum_"++specFullNameHS cls++"_"++el++"() { return "++renderNS (specNS cls)++el++"; }"
         | el<- elems ])

generateFFI :: [String] -> String -> [Spec] -> String
generateFFI mname header specs 
  = unlines (["module "++concat (intersperse "." mname)++" where"
             ,""
             ,"import Foreign"
             ,"import Foreign.C"
             ,"import Data.Typeable"
             ,"import LLVM.FFI.CPP.String"
             ,""]++dts++fns++conv)
  where
    dts = [ case customDecl (specType cs) of
               Nothing -> "data "++hsName (specName cs) ++
                          concat (fmap (\(_,i) -> " a"++show i)
                                  (zip (specCollectTemplateArgs cs) [0..]))++
                          " = "++hsName (specName cs)++" deriving (Typeable)"
               Just decl -> decl
          | cs@Spec { specType = ClassSpec { isInterface = False } } <- nubBy (\x y -> specName x == specName y) specs
          ] ++
          concat [ declareEnumNode node
                 | Spec { specType = EnumSpec node } <- specs ]
    declareEnumNode node = ("data "++enumNodeHSName node++" = "++
                            (concat $ intersperse " | " [ case el of
                                                             Left (name,nd)
                                                               -> name++" "++
                                                                  (enumNodeHSName nd)
                                                             Right l -> enumLeafHSName l
                                                        | el <- enumNodeSubs node ])++
                            " deriving (Typeable,Show,Eq,Ord)"):
                           concat [ declareEnumNode nd
                                  | Left (_,nd) <- enumNodeSubs node ]
    conv = concat
           [ enumConvs cname nd
           | cs@Spec { specType = EnumSpec nd
                     } <- specs
           , let cname = specFullNameHS cs
           ]
    enumConvs cname node
      = ["to"++(enumNodeHSName node)++" :: CInt -> "++(enumNodeHSName node)
        ,"to"++(enumNodeHSName node)++" op"
        ]++concat [ enumToConv cname [] sub
                  | sub <- enumNodeSubs node ]++
        ["","from"++(enumNodeHSName node)++" :: "++(enumNodeHSName node)++" -> CInt"]++
        concat [ enumFromConv cname (enumNodeHSName node) [] sub
               | sub <- enumNodeSubs node ]++
        ["","instance Enum "++enumNodeHSName node++" where"]++
        (zipWith (\lhs rhs -> "  succ "++wrapCons lhs++" = "++wrapCons rhs
                 ) (allHSEnums node) (tail $ allHSEnums node))++
        (zipWith (\lhs rhs -> "  pred "++wrapCons lhs++" = "++wrapCons rhs
                 ) (tail $ allHSEnums node) (allHSEnums node))++
        ["  toEnum = to"++enumNodeHSName node++" . fromIntegral"
        ,"  fromEnum = fromIntegral . from"++enumNodeHSName node]++
        ["  enumFrom "++wrapCons c++" = ["++intercalate "," (fmap wrapCons cs)++"]"
        | c:cs <- tails (allHSEnums node) ]++
        ["  enumFromThen x y = x:enumFrom y"
        ,"  enumFromTo start end = fromTo' (enumFrom start)"
        ,"    where"
        ,"      fromTo' (x:xs) = if x==end then [x] else x:fromTo' xs"
        ,"  enumFromThenTo x y z = x:enumFromTo y z"
        ,"","all"++enumNodeHSName node++" :: ["++enumNodeHSName node++"]"
        ,"all"++enumNodeHSName node++" = ["++intercalate "," (fmap wrapCons $ allHSEnums node)++"]"]
    enumToConv cname prevs (Right leaf)
      = ["  | op == enum_"++cname++"_"++enumLeafCName leaf++
         " = "++concat (intersperse " $ " (prevs++[enumLeafHSName leaf]))]
    enumToConv cname prevs (Left (name,nd))
      = concat
        [ enumToConv cname (prevs++[enumNodeHSName nd]) sub
        | sub <- enumNodeSubs nd ]
    enumFromConv cname hsname prev (Right leaf)
      = ["from"++hsname++" "++wrapCons (prev++[enumLeafHSName leaf])++
         " = enum_"++cname++"_"++enumLeafCName leaf]
    enumFromConv cname hsname prev (Left (name,nd))
      = concat
        [ enumFromConv cname hsname (prev++[name]) sub
        | sub <- enumNodeSubs nd ]
    wrapCons [x] = x
    wrapCons (x:xs) = "("++x++" "++wrapCons xs++")"
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
                                       else ("IO "++).bracket)
                                      (toHaskellType True Nothing rtp),
                                      cname)
                                Constructor { ftConArgs = r }
                                  -> (fmap (\((isO',tp'),n) -> toHaskellType True (if isO'
                                                                                   then Just $ "t"++show n
                                                                                   else Nothing) tp') (zip r [0..]),
                                      "IO "++(bracket $ toHaskellType True Nothing $ toPtr $ specFullType cs),
                                      cname)
                                Destructor { ftOverloadedDestructor = isO }
                                  -> ([toHaskellType True (if isO
                                                           then Just "t"
                                                           else Nothing) $ toPtr $ specFullType cs],
                                      "IO "++(bracket $ toHaskellType True Nothing $ normalT void),
                                      cname)
                                Setter { ftSetType = tp
                                       , ftSetOverloaded = isO }
                                  -> ([toHaskellType True Nothing $ toPtr $ specFullType cs,
                                       toHaskellType True (if isO
                                                           then Just "t"
                                                           else Nothing) tp],
                                      "IO "++(bracket $ toHaskellType True Nothing $ normalT void),
                                      cname)
                                Getter { ftGetType = tp
                                       , ftGetStatic = stat }
                                  -> (if stat
                                      then []
                                      else [toHaskellType True Nothing $ toPtr $ specFullType cs],
                                      "IO "++(bracket $ toHaskellType True Nothing tp),
                                      cname)
                                SizeOf -> ([],toHaskellType True Nothing (normalT size_t),cname)
                                AlignOf -> ([],toHaskellType True Nothing (normalT size_t),cname)
                             ) funs
                   GlobalFunSpec { gfunReturnType = rtp
                                 , gfunArgs = args
                                 , gfunHSName = hsname
                                 } -> [(fmap (\((isO',tp'),n) -> toHaskellType True (if isO'
                                                                                     then Just $ "t"++show n
                                                                                     else Nothing) tp') (zip args [0..]),
                                        "IO "++(bracket $ toHaskellType True Nothing rtp),
                                        hsname)]
                   EnumSpec nd -> [([],toHaskellType True Nothing $ normalT int,"enum_"++specFullNameHS cs++"_"++el)
                                  | el <- allCEnums nd ]
                 , let sig = (concat [ tp ++ " -> " 
                                     | tp <- tps
                                     ]) ++ rtp
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
