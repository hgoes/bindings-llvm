module LLVMSpec where

import Data.Version
import Generator
import CPPType

llvm3_6 :: Version
llvm3_6 = Version { versionBranch = [3,6]
                  , versionTags = []
                  }

llvm3_5 :: Version
llvm3_5 = Version { versionBranch = [3,5]
                  , versionTags = []
                  }

llvm3_4 :: Version
llvm3_4 = Version { versionBranch = [3,4]
                  , versionTags = []
                  }

llvm3_3 :: Version
llvm3_3 = Version { versionBranch = [3,3]
                  , versionTags = []
                  }

llvm3_2 :: Version
llvm3_2 = Version { versionBranch = [3,2]
                  , versionTags = []
                  }

llvm3_1 :: Version
llvm3_1 = Version { versionBranch = [3,1]
                  , versionTags = []
                  }

llvm3_0 :: Version
llvm3_0 = Version { versionBranch = [3,0]
                  , versionTags = []
                  }

llvm2_9 :: Version
llvm2_9 = Version { versionBranch = [2,9]
                  , versionTags = []
                  }

llvm2_8 :: Version
llvm2_8 = Version { versionBranch = [2,8]
                  , versionTags = []
                  }

irInclude :: Version -> String -> String
irInclude ver hdr = if ver >= llvm3_3
                    then "llvm/IR/"++hdr
                    else "llvm/"++hdr

llvm :: Version -> [Spec]
llvm version
  = [Spec { specHeader = "llvm/ADT/StringRef.h"
          , specNS = llvmNS
          , specName = "StringRef"
          , specTemplateArgs = []
          , specType = classSpecCustom "data StringRef = StringRef String deriving (Typeable,Show,Eq,Ord)"
                       [(Constructor { ftConArgs = [] },"newStringRefEmpty")
                       ,(Constructor { ftConArgs = [(False,constT cstring)] },"newStringRef_")
                       ,(Destructor False,"deleteStringRef")
                       ,(memberFun { ftReturnType = constT cstring
                                   , ftName = "data"
                                   },"stringRefData_")
                       ,(memberFun { ftReturnType = normalT size_t
                                   , ftName = "size"
                                   },"stringRefSize_")
                       ]
          }
    ]++
    [if version<llvm3_5
     then Spec { specHeader = "llvm/ADT/OwningPtr.h"
               , specNS = llvmNS
               , specName = "OwningPtr"
               , specTemplateArgs = [rtp]
               , specType = classSpec
                            [(Constructor { ftConArgs = [(False,toPtr rtp)] },"newOwningPtr"++tp)
                            ,(Destructor False,"deleteOwningPtr"++tp)
                            ,(memberFun { ftReturnType = toPtr rtp
                                        , ftName = "take"
                                        },"takeOwningPtr"++tp)]
               }
     else Spec { specHeader = "memory"
               , specNS = [ClassName "std" []]
               , specName = "unique_ptr"
               , specTemplateArgs = [rtp]
               , specType = classSpec
                            [(Constructor { ftConArgs = [(False,toPtr rtp)] },"newUniquePtr"++tp)
                            ,(Destructor False,"deleteUniquePtr"++tp)
                            ,(memberFun { ftReturnType = toPtr rtp
                                        , ftName = "get"
                                        },"getUniquePtr"++tp)
                            ,(memberFun { ftReturnType = toPtr rtp
                                        , ftName = "release"
                                        },"releaseUniquePtr"++tp)]
               }
     | tp <- ["MemoryBuffer"]
    , let rtp = Type [] (llvmType tp)
    ]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/ADT/ArrayRef.h"
                , specNS = llvmNS
                , specName = "ArrayRef"
                , specTemplateArgs = [rtp]
                , specType = classSpec $
                             [(Constructor { ftConArgs = [] },"newArrayRefEmpty"++tp)
                             ,(Constructor { ftConArgs = [(False,toPtr rtp)
                                                         ,(False,normalT size_t)] },"newArrayRef"++tp)
                             ,(Destructor False,"deleteArrayRef"++tp)
                             ,(memberFun { ftReturnType = normalT size_t
                                         , ftName = "size"
                                         },"arrayRefSize"++tp)]++
                             (if version>=llvm3_0
                              then [(memberFun { ftReturnType = normalT bool
                                               , ftName = "equals"
                                               , ftArgs = [(False,normalT $ NamedType llvmNS "ArrayRef" [rtp] False)]
                                               },"arrayRefEquals"++tp)]
                              else [])++
                             [(memberFun { ftReturnType = toConstRef rtp
                                         , ftName = "operator[]"
                                         , ftArgs = [(False,normalT size_t)]
                                         },"arrayRefIndex"++tp)
                             ]
                }
           | (tp,rtp) <- [("Type",normalT $ ptr $ llvmType "Type")
                         ,("Value",normalT $ ptr $ llvmType "Value")
                         ,("CChar",constT $ ptr char)
                         ,("Word64",normalT uint64_t)]
          ]
     else [])++
    concat [[Spec { specHeader = "llvm/ADT/ilist.h"
                  , specNS = llvmNS
                  , specName = "iplist"
                  , specTemplateArgs = [rtp]
                  , specType = classSpec
                               [(Constructor { ftConArgs = [] },"new"++tp++"List")
                               ,(Destructor False,"delete"++tp++"List")
                               ,(memberFun { ftReturnType = normalT size_t
                                           , ftName = "size"
                                           },"list"++tp++"Size")
                               ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "ilist_iterator" [rtp] False
                                           , ftName = "begin"
                                           },"list"++tp++"Begin")
                               ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "ilist_iterator" [rtp] False
                                           , ftName = "end"
                                           },"list"++tp++"End")
                               ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "ilist_iterator" [rtp] False
                                           , ftName = "insert"
                                           , ftArgs = [(False,normalT $ NamedType llvmNS "ilist_iterator" [rtp] False)
                                                      ,(False,normalT $ ptr $ llvmType tp)]
                                           },"list"++tp++"Insert")
                               ,(memberFun { ftReturnType = normalT $ ptr $ llvmType tp
                                           , ftName = "remove"
                                           , ftArgs = [(False,normalT $ ref $
                                                              NamedType llvmNS "ilist_iterator" [rtp] False)]
                                           },"list"++tp++"Remove")
                               ,(memberFun { ftName = "push_front"
                                           , ftArgs = [(False,toPtr rtp)]
                                           },"list"++tp++"PushFront")
                               ,(memberFun { ftName = "push_back"
                                           , ftArgs = [(False,toPtr rtp)]
                                           },"list"++tp++"PushBack")
                               ]
                  }
            ,Spec { specHeader = "llvm/ADT/ilist.h"
                  , specNS = llvmNS
                  , specName = "ilist_iterator"
                  , specTemplateArgs = [rtp]
                  , specType = classSpec
                               [(memberFun { ftReturnType = toPtr rtp
                                           , ftName = "operator->"
                                           },"listIterator"++tp++"Deref")
                               ,(memberFun { ftReturnType = normalT $ RefType $ 
                                                            NamedType llvmNS "ilist_iterator" [rtp] False
                                           , ftName = "operator++"
                                           },"listIterator"++tp++"Next")
                               ,(memberFun { ftReturnType = normalT $ RefType $ 
                                                            NamedType llvmNS "ilist_iterator" [rtp] False
                                           , ftName = "operator--"
                                           },"listIterator"++tp++"Prev")
                               ,(memberFun { ftReturnType = normalT bool
                                           , ftName = "operator=="
                                           , ftArgs = [(False,constT $ RefType $ 
                                                             NamedType llvmNS "ilist_iterator" [rtp] False)] 
                                           },"listIterator"++tp++"Eq")
                               ,(memberFun { ftReturnType = normalT bool
                                           , ftName = "operator!="
                                           , ftArgs = [(False,constT $ RefType $ 
                                                             NamedType llvmNS "ilist_iterator" [rtp] False)] 
                                           },"listIterator"++tp++"NEq")]
                  }
            ,Spec { specHeader = "llvm/ADT/ilist.h"
                  , specNS = llvmNS
                  , specName = "ilist_node"
                  , specTemplateArgs = [rtp]
                  , specType = interfaceSpec
                               [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "ilist_node" [rtp] True
                                           , ftName = "getPrevNode"
                                           },"listNode"++tp++"Prev")
                               ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "ilist_node" [rtp] True
                                           , ftName = "getNextNode"
                                           },"listNode"++tp++"Next")]
                  }
            ]
            | tp <- ["Function","Instruction","BasicBlock","GlobalVariable","Argument","NamedMDNode"]
           , let rtp = Type [] (NamedType llvmNS tp [] False)
           ]++
    concat [[Spec { specHeader = "llvm/ADT/SetVector.h"
                  , specNS = llvmNS
                  , specName = "SetVector"
                  , specTemplateArgs = [rtp]
                  , specType = classSpec
                               [(memberFun { ftReturnType = normalT bool
                                           , ftName = "empty"
                                           },"setVector"++tp++"Empty")
                               ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [] False
                                           , ftName = "begin"
                                           },"setVector"++tp++"Begin")
                               ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [] False
                                           , ftName = "end"
                                           },"setVector"++tp++"End")
                               ]
                  }
            ,Spec { specHeader = "vector"
                  , specNS = [ClassName "std" []]
                  , specName = "vector"
                  , specTemplateArgs = [rtp]
                  , specType = classSpec
                               [(memberFun { ftReturnType = constT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [] False
                                           , ftName = "begin"
                                           , ftOverloaded = True
                                           },"vector"++tp++"Begin")
                               ,(memberFun { ftReturnType = constT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [] False
                                           , ftName = "end"
                                           , ftOverloaded = True
                                           },"vector"++tp++"End")
                               ,(Constructor [(False,toPtr rtp),(False,toPtr rtp)],"newVector"++tp)
                               ,(memberFun { ftName = "clear"
                                           },"vector"++tp++"Clear")
                               ,(memberFun { ftName = "push_back"
                                           , ftArgs = [(False,toRef rtp)]
                                           },"vector"++tp++"PushBack")
                               ,(memberFun { ftName = "resize"
                                           , ftArgs = [(False,normalT unsigned)]
                                           },"vector"++tp++"Resize")
                               ,(memberFun { ftReturnType = toRef rtp
                                           , ftName = "operator[]"
                                           , ftArgs = [(False,normalT size_t)]
                                           },"vector"++tp++"Index")
                               ,(memberFun { ftReturnType = normalT size_t
                                           , ftName = "size"
                                           },"vector"++tp++"Size")]
                  }
            ,Spec { specHeader = "vector"
                  , specNS = [ClassName "std" [],ClassName "vector" [rtp]]
                  , specName = "const_iterator"
                  , specTemplateArgs = []
                  , specType = classSpec
                               [(memberFun { ftReturnType = rtp
                                           , ftName = "operator*"
                                           },"vectorIterator"++tp++"Deref")
                               ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [] False
                                           , ftName = "operator++"
                                           },"vectorIterator"++tp++"Next")
                               ]
                  }
            ,Spec { specHeader = "vector"
                  , specNS = []
                  , specName = "operator=="
                  , specTemplateArgs = []
                  , specType = GlobalFunSpec { gfunReturnType = normalT bool
                                             , gfunArgs = [(False,constT $ ref $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [] False)
                                                          ,(False,constT $ ref $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [] False)]
                                             , gfunHSName = "vectorIterator"++tp++"Eq"
                                             }
                  }
            ]
            | (tp,rtp) <- [("Type",normalT $ ptr $ llvmType "Type")
                         ,("Loop",normalT $ ptr $ llvmType "Loop")
                         ,("BasicBlock",normalT $ ptr $ llvmType "BasicBlock")
                         ,("DominatorTree",normalT $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType "BasicBlock"] False)
                         ,("GenericValue",normalT $ llvmType "GenericValue")]
           ]++
    [Spec { specHeader = "llvm/ADT/SmallVector.h"
          , specNS = llvmNS
          , specName = "SmallVector"
          , specTemplateArgs = [rtp,TypeInt 16]
          , specType = classSpec
                       [(Constructor [],"newSmallVector"++tp)
                       ,(Destructor False,"deleteSmallVector"++tp)
                       ,(memberFun { ftReturnType = normalT size_t
                                   , ftName = "size"
                                   , ftOverloaded = True
                                   },"smallVectorSize"++tp)
                       ,(memberFun { ftReturnType = toPtr rtp
                                   , ftName = "data"
                                   , ftOverloaded = True
                                   },"smallVectorData"++tp)]
          }
     | (tp,rtp) <- [("Loop",normalT $ ptr $ llvmType "Loop")
                   ,("Edge",normalT $ NamedType [ClassName "std" []] "pair" [normalT $ ptr $ llvmType "BasicBlock"
                                                                            ,normalT $ ptr $ llvmType "BasicBlock"] False)
                   ,("MDNodePair",normalT $ NamedType [ClassName "std" []] "pair" [normalT unsigned
                                                                                  ,normalT $ ptr $ llvmType "MDNode"] False)
                   ,("StringRef",normalT $ llvmType "StringRef")
                   ]
    ]++
    [Spec { specHeader = "utility"
          , specNS = [ClassName "std" []]
          , specName = "pair"
          , specTemplateArgs = [rtp1,rtp2]
          , specType = classSpecCustom "data Pair a b = Pair a b deriving (Typeable,Show,Eq,Ord)"
                       [(Getter { ftGetType = rtp1
                                , ftGetVar = "first"
                                , ftGetStatic = False
                                },"pairFirst"++tp1++"_"++tp2)
                       ,(Getter { ftGetType = rtp2
                                , ftGetVar = "second"
                                , ftGetStatic = False
                                },"pairSecond"++tp1++"_"++tp2)
                       ,(Setter { ftSetVar = "first"
                                , ftSetType = rtp1
                                },"pairSetFirst"++tp1++"_"++tp2)
                       ,(Setter { ftSetVar = "second"
                                , ftSetType = rtp2
                                },"pairSetSecond"++tp1++"_"++tp2)
                       ,(SizeOf,"sizeofPair"++tp1++"_"++tp2)
                       ,(AlignOf,"alignofPair"++tp1++"_"++tp2)
                       ]
          }
     | (tp1,rtp1,tp2,rtp2) <- [("BasicBlock",normalT $ ptr $ llvmType "BasicBlock","BasicBlock",normalT $ ptr $ llvmType "BasicBlock")
                             ,("Unsigned",normalT unsigned,"MDNode",normalT $ ptr $ llvmType "MDNode")] ]++
       [Spec { specHeader = "llvm/ADT/APFloat.h"
             , specNS = llvmNS
             , specName = "APFloat"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT double
                                      , ftName = "convertToDouble"
                                      },"apFloatConvertToDouble")]
             }
       ,Spec { specHeader = "llvm/ADT/APInt.h"
             , specNS = llvmNS
             , specName = "APInt"
             , specTemplateArgs = []
             , specType = classSpecCustom "data APInt = APInt Int Integer deriving (Typeable,Show,Eq,Ord)"
                          [(Constructor [(False,normalT unsigned)
                                        ,(False,normalT uint64_t)
                                        ,(False,normalT bool)]
                           ,"newAPIntLimited")
                          ,if version>=llvm3_0
                           then (Constructor [(False,normalT unsigned)
                                             ,(False,normalT $ NamedType llvmNS "ArrayRef"
                                                        [normalT uint64_t] False)]
                                ,"newAPInt")
                           else (Constructor [(False,normalT unsigned)
                                             ,(False,normalT unsigned)
                                             ,(False,constT $ ptr $ uint64_t)]
                                ,"newAPInt")
                          ,(Constructor [(False,normalT unsigned)
                                        ,(False,normalT $ llvmType "StringRef")
                                        ,(False,normalT uint8_t)]
                           ,"newAPIntFromString")
                          ,(Destructor False,"deleteAPInt")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getBitWidth"
                                      },"apIntGetBitWidth")
                          ,(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getZExtValue"
                                      },"apIntGetZExtValue")
                          ,(memberFun { ftReturnType = normalT int64_t
                                      , ftName = "getSExtValue"
                                      },"apIntGetSExtValue")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumWords"
                                      },"apIntGetNumWords")
                          ,(memberFun { ftReturnType = constT $ ptr uint64_t
                                      , ftName = "getRawData"
                                      },"apIntGetRawData")]
             }
       ,Spec { specHeader = if version<llvm3_5
                            then "llvm/Support/DebugLoc.h"
                            else "llvm/IR/DebugLoc.h"
             , specNS = llvmNS
             , specName = "DebugLoc"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(Constructor { ftConArgs = [] },"newDebugLoc")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isUnknown"
                                      },"debugLocIsUnknown")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getLine"
                                      },"debugLocGetLine")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getCol"
                                      },"debugLocGetCol")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "MDNode"
                                      , ftName = "getScope"
                                      , ftArgs = [(False,constT $ ref $ llvmType "LLVMContext")]
                                      },"debugLocGetScope")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "MDNode"
                                      , ftName = "getInlinedAt"
                                      , ftArgs = [(False,constT $ ref $ llvmType "LLVMContext")]
                                      },"debugLocGetInlinedAt")]++
                          (if version>=llvm3_0
                           then [(memberFun { ftName = "dump" 
                                            , ftArgs = [(False,constT $ ref $ llvmType "LLVMContext")]
                                            },"debugLocDump")]
                           else [])
             }
       ]++
       [Spec { specHeader = irInclude version "Type.h"
             , specNS = llvmNS
             , specName = "Type"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT void
                                      , ftName = "dump"
                                      , ftOverloaded = True
                                      },"typeDump_")
                          ,(memberFun { ftReturnType = normalT $ ref $ llvmType "LLVMContext"
                                      , ftName = "getContext"
                                      , ftOverloaded = True
                                      },"typeGetContext_")
                          ]++
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "is"++tp++"Ty"
                                      , ftOverloaded = True
                                      , ftPure = True
                                      },"is"++tp++"Ty_")
                          | tp <- ["Void"]++(if version>=llvm3_1
                                             then ["Half"]
                                             else [])++
                                  ["Float","Double","X86_FP80","FP128","PPC_FP128"
                                  ,"FloatingPoint"]++
                                  (if version>=llvm2_9
                                   then ["X86_MMX"]
                                   else [])++
                                  ["Label","Metadata"]
                          ]++
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Type"
                                      , ftName = "get"++tp++"Ty"
                                      , ftStatic = True
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")]
                                      },"get"++tp++"Type")
                          | tp <- ["Void"]++(if version>=llvm3_1
                                             then ["Half"]
                                             else [])++
                                  ["Float","Double","X86_FP80","FP128","PPC_FP128"]++
                                  (if version>=llvm2_9
                                   then ["X86_MMX"]
                                   else [])++
                                  ["Label","Metadata"]
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "IntegerType"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getBitWidth"
                                      },"getBitWidth_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "IntegerType" 
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ RefType $ llvmType "LLVMContext")
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },"getIntegerType_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "CompositeType"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Type"
                                      , ftName = "getTypeAtIndex"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },"compositeTypeGetTypeAtIndex_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "indexValid"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },"compositeTypeIndexValid_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "SequentialType"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Type"
                                      , ftName = "getElementType"
                                      , ftOverloaded = True
                                      },"sequentialTypeGetElementType_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "ArrayType"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getNumElements"
                                      },"arrayTypeGetNumElements_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "PointerType"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAddressSpace"
                                      },"pointerTypeGetAddressSpace_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "PointerType"
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },"pointerTypeGet_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "VectorType"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumElements"
                                      },"vectorTypeGetNumElements_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "VectorType"
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },"vectorTypeGet_")
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "StructType"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isPacked"
                                      },"structTypeIsPacked")]++
                          (if version>=llvm3_0
                           then [(memberFun { ftReturnType = normalT bool
                                            , ftName = "hasName"
                                            },"structTypeHasName")
                                ,(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                            , ftName = "getName"
                                            },"structTypeGetName")]
                           else [])++
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumElements"
                                      },"structTypeGetNumElements_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Type"
                                      , ftName = "getElementType"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"structTypeGetElementType_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,if version>=llvm3_0
                                                         then normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Type"] False
                                                         else constT $ ref $ NamedType [ClassName "std" []] "vector" [constT $ ptr $ llvmType "Type"] False)
                                                 ,(False,normalT bool)]
                                      , ftStatic = True
                                      },"newStructType")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "create"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,if version>=llvm3_0
                                                         then normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Type"] False
                                                         else constT $ ref $ NamedType [ClassName "std" []] "vector" [constT $ ptr $ llvmType "Type"] False)
                                                 ,(False,normalT $ llvmType "StringRef")
                                                 ,(False,normalT bool)]
                                      , ftStatic = True
                                      },"newNamedStructType")
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "FunctionType"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVarArg"
                                      },"functionTypeIsVarArg")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumParams"
                                      },"functionTypeGetNumParams_")
                          ,(memberFun { ftReturnType = normalT (ptr $ llvmType "Type")
                                      , ftName = "getParamType"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"functionTypeGetParamType_")
                          ,(memberFun { ftReturnType = normalT (ptr $ llvmType "Type")
                                      , ftName = "getReturnType"
                                      },"functionTypeGetReturnType")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionType"
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")
                                                 ,(False,if version>=llvm3_0
                                                         then normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Type"] False
                                                         else constT $ ref $ NamedType [ClassName "std" []] "vector" [constT $ ptr $ llvmType "Type"] False)
                                                 ,(False,normalT bool)
                                                 ]
                                      , ftStatic = True
                                      },"newFunctionType_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Value.h"
             , specNS = llvmNS
             , specName = "Value"
             , specTemplateArgs = []
             , specType = let iter = if version<llvm3_5
                                     then NamedType llvmNS "value_use_iterator"
                                          [normalT $ llvmType "User"] False
                                     else NamedType [ClassName "llvm" []
                                                    ,ClassName "Value" []]
                                          "use_iterator"
                                          [] False
                          in classSpec
                             [(Destructor True,"deleteValue_")
                             ,(memberFun { ftName = "dump"
                                         , ftOverloaded = True
                                         },"valueDump_")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "hasName"
                                         , ftOverloaded = True
                                         },"hasName_")
                             ,(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                         , ftName = "getName"
                                         , ftOverloaded = True
                                         },"getName_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Type"
                                         , ftName = "getType"
                                         , ftOverloaded = True
                                         },"valueGetType_")
                             ,(memberFun { ftReturnType = normalT iter
                                         , ftName = "use_begin"
                                         , ftOverloaded = True
                                         },"valueUseBegin_")
                             ,(memberFun { ftReturnType = normalT iter
                                         , ftName = "use_end"
                                         , ftOverloaded = True
                                         },"valueUseEnd_")
                             ,(memberFun { ftReturnType = normalT void
                                         , ftName = "replaceAllUsesWith"
                                         , ftArgs = [(True,normalT $ ptr $ llvmType "Value")]
                                         , ftOverloaded = True
                                         },"valueReplaceAllUsesWith_")
                             ]
             }
       ,Spec { specHeader = irInclude version "Argument.h"
             , specNS = llvmNS
             , specName = "Argument"
             , specTemplateArgs = []
             , specType = classSpec 
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                      , ftName = "getParent"
                                      },"argumentGetParent")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getArgNo"
                                      },"argumentGetArgNo_")
                          ,(Constructor [(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,constT $ ref $ llvmType "Twine")
                                        ,(False,normalT $ ptr $ llvmType "Function")
                                        ],"createArgument_")
                          ]
             }
       ,Spec { specHeader = irInclude version "BasicBlock.h"
             , specNS = llvmNS
             , specName = "BasicBlock"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                      , ftName = "getParent"
                                      },"basicBlockGetParent")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType llvmNS "iplist" 
                                                       [normalT $ llvmType "Instruction"]
                                                       False
                                      , ftName = "getInstList"
                                      },"getInstList")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "TerminatorInst"
                                      , ftName = "getTerminator"
                                      },"getTerminator")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "Create"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,normalT $ ref $ llvmType "Twine")
                                                 ,(False,normalT $ ptr $ llvmType "Function")
                                                 ,(False,normalT $ ptr $ llvmType "BasicBlock")]
                                      , ftStatic = True
                                      },"createBasicBlock")
                          ,(Destructor False,"deleteBasicBlock")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Instruction"
                                      , ftName = "getFirstNonPHI"
                                      },"getFirstNonPHI")
                          ]
             }
       ,Spec { specHeader = irInclude version "InlineAsm.h"
             , specNS = llvmNS
             , specName = "InlineAsm"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "hasSideEffects"
                                      },"inlineAsmHasSideEffects")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isAlignStack"
                                      },"inlineAsmIsAlignStack")]++
                          (if version>=llvm3_2
                           then [(memberFun { ftReturnType = normalT $ EnumType [ClassName "llvm" []
                                                                                ,ClassName "InlineAsm" []] "AsmDialect"
                                            , ftName = "getDialect"
                                            },"inlineAsmGetDialect_")]
                           else [])++
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "PointerType"
                                      , ftName = "getType"
                                      },"inlineAsmGetType")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionType"
                                      , ftName = "getFunctionType"
                                      },"inlineAsmGetFunctionType")
                          ,(memberFun { ftReturnType = constT $ ref $ NamedType [ClassName "std" []] "string" [] False
                                      , ftName = "getAsmString"
                                      },"inlineAsmGetAsmString")
                          ,(memberFun { ftReturnType = constT $ ref $ NamedType [ClassName "std" []] "string" [] False
                                      , ftName = "getConstraintString"
                                      },"inlineAsmGetConstraintString")]
             }]++
       (if version>=llvm3_2
        then [Spec { specHeader = irInclude version "InlineAsm.h"
                   , specNS = [ClassName "llvm" []
                              ,ClassName "InlineAsm" []]
                   , specName = "AsmDialect"
                   , specTemplateArgs = []
                   , specType = EnumSpec $
                                EnumNode "AsmDialect"
                                [Right $ EnumLeaf "AD_ATT" "AsmDialectATT"
                                ,Right $ EnumLeaf "AD_Intel" "AsmDialectIntel"]
                   }]
        else [])++
       (if version>=llvm3_6
        then [Spec { specHeader = irInclude version "Metadata.h"
                   , specNS = llvmNS
                   , specName = "Metadata"
                   , specTemplateArgs = []
                   , specType = classSpec
                                [(memberFun { ftName = "dump"
                                            , ftOverloaded = True
                                            },"metadataDump_")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getMetadataID"
                                            , ftOverloaded = True
                                            },"metadataGetID_")]
                   }]
        else [])++
       [Spec { specHeader = irInclude version "Metadata.h"
             , specNS = llvmNS
             , specName = "MDNode"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getOperand"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"mdNodeGetOperand")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumOperands"
                                      },"mdNodeGetNumOperands")]++
                          (if version<llvm3_6
                           then [(memberFun { ftReturnType = normalT bool
                                            , ftName = "isFunctionLocal"
                                            },"mdNodeIsFunctionLocal")
                                ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                            , ftName = "getFunction"
                                            },"mdNodeGetFunction")]
                           else [])++
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "MDNode"
                                      , ftName = "get"
                                      , ftArgs = if version>=llvm2_9
                                                 then [(False,normalT $ ref $ llvmType "LLVMContext")
                                                      ,(False,normalT $ NamedType llvmNS "ArrayRef"
                                                              [normalT $ ptr $ llvmType "Value"] False)]
                                                 else [(False,normalT $ ref $ llvmType "LLVMContext")
                                                      ,(False,normalT $ ptr $ ptr $ llvmType "Value")
                                                      ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },"newMDNode")]
             }
       ,Spec { specHeader = irInclude version "Metadata.h"
             , specNS = llvmNS
             , specName = "NamedMDNode"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Module"
                                      , ftName = "getParent"
                                      },"namedMDNodeGetParent")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "MDNode"
                                      , ftName = "getOperand"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"namedMDNodeGetOperand")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumOperands"
                                      },"namedMDNodeGetNumOperands")
                          ,(memberFun { ftName = "addOperand"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "MDNode")]
                                      },"namedMDNodeAddOperand")
                          ,(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                      , ftName = "getName"
                                      },"namedMDNodeGetName")]
             }
       ,Spec { specHeader = irInclude version "Metadata.h"
             , specNS = llvmNS
             , specName = "MDString"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                      , ftName = "getString"
                                      },"mdStringGetString")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "MDString"
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,normalT $ llvmType "StringRef")]
                                      , ftStatic = True },"newMDString")]
             }
       ,Spec { specHeader = "llvm/CodeGen/PseudoSourceValue.h"
             , specNS = llvmNS
             , specName = "FixedStackPseudoSourceValue"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = irInclude version "Constant.h"
             , specNS = llvmNS
             , specName = "Constant"
             , specTemplateArgs = []
             , specType = classSpec $
                          (if version>=llvm3_1
                           then [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                            , ftName = "getAggregateElement"
                                            , ftArgs = [(False,normalT unsigned)]
                                            , ftOverloaded = True
                                            },"constantGetAggregateElement_")]
                           else [])++
                          (if version >= llvm3_3
                           then [(memberFun { ftReturnType = normalT bool
                                            , ftName = "isNullValue"
                                            },"isNullValue")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "canTrap"
                                            },"canTrap")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isThreadDependent"
                                            },"isThreadDependent")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isConstantUsed"
                                            },"isConstantUsed")
                                ]
                           else [])
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "BlockAddress"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantArray"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "ArrayType"
                                      , ftName = "getType"
                                      },"constantArrayGetType")]
             }
       ]++
    (if version>=llvm3_1
     then [Spec { specHeader = irInclude version "Constants.h"
                , specNS = llvmNS
                , specName = "ConstantDataSequential"
                , specTemplateArgs = []
                , specType = classSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ llvmType "SequentialType"
                                         , ftName = "getType"
                                         , ftOverloaded = True
                                         },"constantDataSequentialGetType")
                             ,(memberFun { ftReturnType = normalT unsigned
                                         , ftName = "getNumElements"
                                         , ftOverloaded = True
                                         },"constantDataSequentialGetNumElements_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                         , ftName = "getElementAsConstant"
                                         , ftOverloaded = True
                                         , ftArgs = [(False,normalT unsigned)]
                                         },"constantDataSequentialGetElementAsConstant_")]
                }
          ,Spec { specHeader = irInclude version "Constants.h"
                , specNS = llvmNS
                , specName = "ConstantDataArray"
                , specTemplateArgs = []
                , specType = classSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ llvmType "ArrayType"
                                         , ftName = "getType"
                                         },"constantDataArrayGetType")]
                }
          ,Spec { specHeader = irInclude version "Constants.h"
                , specNS = llvmNS
                , specName = "ConstantDataVector"
                , specTemplateArgs = []
                , specType = classSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ llvmType "VectorType"
                                         , ftName = "getType"
                                         },"constantDataVectorGetType")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantExpr"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getOpcode"
                                      , ftOverloaded = True
                                      },"constantExprGetOpcode_")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getPredicate"
                                      , ftOverloaded = True
                                      },"constantExprGetPredicate_")]++
                          (if version>=llvm3_3
                           then [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Instruction"
                                            , ftName = "getAsInstruction"
                                            , ftOverloaded = True
                                            },"constantExprAsInstruction_")]
                           else [])++
                          [(memberFun { ftStatic = True
                                      , ftReturnType = normalT $ ptr $ llvmType "Constant"
                                      , ftName = "getPointerCast"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Constant")
                                                 ,(True,normalT $ ptr $ llvmType "Type")]
                                      },"getPointerCast_")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantFP"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = constT $ ref $ llvmType "APFloat"
                                      , ftName = "getValueAPF"
                                      },"constantFPGetValueAPF")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantInt"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "IntegerType"
                                      , ftName = "getType"
                                      },"constantIntGetType")
                          ,(memberFun { ftReturnType = constT $ ref $ llvmType "APInt"
                                      , ftName = "getValue"
                                      },"constantIntGetValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ConstantInt"
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,constT $ ref $ llvmType "APInt")]
                                      , ftStatic = True
                                      },"createConstantInt")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantPointerNull"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "PointerType"
                                      , ftName = "getType"
                                      },"constantPointerNullGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantStruct"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "getType"
                                      },"constantStructGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantVector"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "VectorType"
                                      , ftName = "getType"
                                      },"constantVectorGetType")]
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = llvmNS
             , specName = "GlobalValue"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "PointerType"
                                      , ftName = "getType"
                                      , ftOverloaded = True
                                      },"globalValueGetType")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isDeclaration"
                                      , ftOverloaded = True
                                      },"globalValueIsDeclaration_")]
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = [ClassName "llvm" []
                        ,ClassName "GlobalValue" []]
             , specName = "LinkageTypes"
             , specTemplateArgs = []
             , specType = EnumSpec $
                          EnumNode "LinkageTypes"
                          [Right $ EnumLeaf name name
                          | name <- ["ExternalLinkage"             -- Externally visible function.
                                    ,"AvailableExternallyLinkage"  -- Available for inspection, not emission.
                                    ,"LinkOnceAnyLinkage"          -- Keep one copy of function when linking (inline)
                                    ,"LinkOnceODRLinkage"          -- Same, but only replaced by something equivalent.
                                    ,"WeakAnyLinkage"              -- Keep one copy of named function when linking (weak)
                                    ,"WeakODRLinkage"              -- Same, but only replaced by something equivalent.
                                    ,"AppendingLinkage"            -- Special purpose, only applies to global arrays.
                                    ,"InternalLinkage"             -- Rename collisions when linking (static functions).
                                    ,"PrivateLinkage"              -- Like Internal, but omit from symbol table.
                                    ,"ExternalWeakLinkage"         -- ExternalWeak linkage description.
                                    ,"CommonLinkage"               -- Tentative definitions.
                                    ]]
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = llvmNS
             , specName = "GlobalAlias"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = llvmNS
             , specName = "GlobalVariable"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isConstant"
                                      },"globalVariableIsConstant")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isThreadLocal"
                                      },"globalVariableIsThreadLocal")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                      , ftName = "getInitializer"
                                      },"globalVariableGetInitializer")
                          ]
             }
       ,Spec { specHeader = irInclude version "Function.h"
             , specNS = llvmNS
             , specName = "Function"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVarArg"
                                      },"functionIsVarArg")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType llvmNS "iplist" 
                                                       [normalT $ llvmType "BasicBlock"]
                                                       False
                                      , ftName = "getBasicBlockList"
                                      },"getBasicBlockList")
                          ,(memberFun { ftReturnType = normalT $ RefType $ llvmType "BasicBlock"
                                      , ftName = "getEntryBlock"
                                      },"getEntryBlock")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionType"
                                      , ftName = "getFunctionType"
                                      },"functionGetFunctionType")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType llvmNS "iplist"
                                                       [normalT $ llvmType "Argument"]
                                                       False
                                      , ftName = "getArgumentList"
                                      },"functionGetArgumentList")
                          ,(Destructor False,"deleteFunction")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                      , ftName = "Create"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "FunctionType")
                                                 ,(False,normalT $ EnumType
                                                         [ClassName "llvm" []
                                                         ,ClassName "GlobalValue" []]
                                                         "LinkageTypes")
                                                 ,(False,constT $ ref $ llvmType "Twine")
                                                 ,(False,normalT $ ptr $ llvmType "Module")
                                                 ]
                                      , ftStatic = True
                                      },"createFunction_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "UndefValue"
             , specTemplateArgs = []
             , specType = classSpec []
             }]++
    (if version>=llvm3_5
     then [Spec { specHeader = "llvm/Support/ErrorOr.h"
                , specNS = llvmNS
                , specName = "ErrorOr"
                , specTemplateArgs = [normalT rtp]
                , specType = classSpec
                             [(memberFun { ftReturnType = normalT bool
                                         , ftName = "operator bool"
                                         },"errorOrIsSuccess"++tp)
                             ,(memberFun { ftReturnType = normalT $ NamedType
                                                          [ClassName "std" []]
                                                          "error_code" []
                                                          False
                                         , ftName = "getError"
                                         },"errorOrGetError"++tp)
                             ,(memberFun { ftReturnType = normalT $ ref rtp
                                         , ftName = "get"
                                         },"errorOrGet"++tp)
                             ]
                }
           | (tp,rtp) <- [("MemoryBuffer",
                           NamedType [ClassName "std" []] "unique_ptr"
                           [normalT $ llvmType "MemoryBuffer"]
                           False)]
          ]
     else [])++
    (if version>=llvm2_9
     then [Spec { specHeader = if version<llvm3_5
                               then "llvm/Support/system_error.h"
                               else "system_error"
                , specNS = if version<llvm3_5
                           then llvmNS
                           else [ClassName "std" []]
                , specName = "error_code"
                , specTemplateArgs = []
                , specType = classSpec
                             [(Destructor False,"deleteErrorCode")
                             ,(memberFun { ftReturnType = normalT int
                                         , ftName = "value"
                                         },"errorCodeValue")
                             ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" []] "string" [] False
                                         , ftName = "message"
                                         },"errorCodeMessage_")]
                }]
     else [])++
       [Spec { specHeader = "llvm/Support/MemoryBuffer.h"
             , specNS = llvmNS
             , specName = "MemoryBuffer"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Destructor False,"deleteMemoryBuffer")
                          ,(memberFun { ftReturnType = normalT size_t 
                                      , ftName = "getBufferSize"
                                      },"getBufferSize_")
                          ,if version>=llvm2_9
                           then (memberFun { ftReturnType = if version<llvm3_5
                                                            then normalT (llvmType "error_code")
                                                            else normalT $
                                                                 NamedType llvmNS
                                                                 "ErrorOr"
                                                                 [normalT $
                                                                  NamedType [ClassName "std" []]
                                                                  "unique_ptr"
                                                                  [normalT $ llvmType "MemoryBuffer"]
                                                                  False]
                                                                 False
                                           , ftName = "getFile"
                                           , ftArgs = [(False,normalT (llvmType "StringRef"))]++
                                                      (if version<llvm3_5
                                                       then [(False,normalT $
                                                                    RefType $
                                                                    NamedType llvmNS
                                                                    "OwningPtr" 
                                                                    [normalT (llvmType "MemoryBuffer")]
                                                                    False)]
                                                       else [])++
                                                      [(False,normalT int64_t)]++
                                                      (if version>=llvm3_0
                                                       then [(False,normalT bool)]
                                                       else [])
                                           , ftStatic = True
                                           },"getFileMemoryBuffer")
                           else (memberFun { ftReturnType = normalT $ ptr $ llvmType "MemoryBuffer"
                                           , ftName = "getFile"
                                           , ftArgs = [(False,normalT (llvmType "StringRef"))]
                                           , ftStatic = True
                                           },"getFileMemoryBuffer")
                          ,if version>=llvm2_9
                           then (memberFun { ftReturnType = if version<llvm3_5
                                                            then if version<=llvm2_8
                                                                 then normalT $ ptr $ llvmType "MemoryBuffer"
                                                                 else normalT (llvmType "error_code")
                                                            else normalT $
                                                                 NamedType llvmNS
                                                                 "ErrorOr"
                                                                 [normalT $
                                                                  NamedType [ClassName "std" []]
                                                                  "unique_ptr"
                                                                  [normalT $ llvmType "MemoryBuffer"]
                                                                  False]
                                                                 False
                                           , ftName = "getSTDIN"
                                           , ftArgs = if version<llvm3_5 && version>=llvm2_9
                                                      then [(False,normalT $ ref $
                                                                   NamedType llvmNS
                                                                   "OwningPtr"
                                                                   [normalT $ llvmType "MemoryBuffer"]
                                                                   False)]
                                                      else []
                                           , ftStatic = True
                                           },"getStdInMemoryBuffer")
                           else (memberFun { ftReturnType = normalT $ ptr $ llvmType "MemoryBuffer"
                                           , ftName = "getSTDIN"
                                           , ftArgs = []
                                           , ftStatic = True
                                           },"getStdInMemoryBuffer")
                          ]
             }
       ,Spec { specHeader = "llvm/Support/SourceMgr.h"
             , specNS = llvmNS
             , specName = "SMDiagnostic"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [],"newSMDiagnostic")
                          ,(Destructor False,"deleteSMDiagnostic")
                          ,(memberFun { ftReturnType = normalT (llvmType "StringRef")
                                      , ftName = "getFilename"
                                      },"getFilename")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getLineNo"
                                      },"getLineNo_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getColumnNo"
                                      },"getColumnNo_")
                          ]
             }
       ,Spec { specHeader = irInclude version "LLVMContext.h"
             , specNS = llvmNS
             , specName = "LLVMContext"
             , specTemplateArgs = []
             , specType = classSpec [(Constructor [],"newLLVMContext")
                                    ,(Destructor False,"deleteLLVMContext")
                                    ,(memberFun { ftName = "getMDKindNames"
                                                , ftArgs = [(False,normalT $ NamedType llvmNS 
                                                                   "SmallVector"
                                                                   [normalT $ llvmType "StringRef"
                                                                   ,TypeInt 16]
                                                                   False)]
                                                },"llvmContextGetMDKindNames")
                                    ,(memberFun { ftReturnType = normalT unsigned
                                                , ftName = "getMDKindID"
                                                , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                                },"llvmContextGetMDKindID")]
             }
       ,Spec { specHeader = irInclude version "LLVMContext.h"
             , specNS = llvmNS
             , specName = "getGlobalContext"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ref $ llvmType "LLVMContext"
                                        , gfunArgs = []
                                        , gfunHSName = "getGlobalContext"
                                        }
             }
       ,Spec { specHeader = irInclude version "Module.h"
             , specNS = llvmNS
             , specName = "Module"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(Constructor [(False,normalT $ llvmType "StringRef")
                                        ,(False,normalT $ ref $ llvmType "LLVMContext")
                                        ],"newModule")
                          ,(Destructor False,"deleteModule")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "dump"
                                      },"moduleDump")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType llvmNS "iplist"
                                                       [normalT $ NamedType llvmNS "Function" [] False]
                                                       False
                                      , ftName = "getFunctionList"
                                      },"moduleGetFunctionList")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType llvmNS "iplist"
                                                       [normalT $ llvmType "GlobalVariable"]
                                                       False
                                      , ftName = "getGlobalList"
                                      },"moduleGetGlobalList")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "getTypeByName"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      },"moduleGetTypeByName")
                          ,(memberFun { ftReturnType = normalT $ ref $ llvmType "LLVMContext"
                                      , ftName = "getContext"
                                      },"moduleGetContext")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "GlobalValue"
                                      , ftName = "getNamedValue"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      },"moduleGetNamedValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "NamedMDNode"
                                      , ftName = "getNamedMetadata"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Twine")]
                                      },"moduleGetNamedMetadata")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "NamedMDNode"
                                      , ftName = "getOrInsertNamedMetadata"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      },"moduleGetOrInsertNamedMetadata")
                          ,(memberFun { ftName = "eraseNamedMetadata"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "NamedMDNode")]
                                      },"moduleEraseNamedMetadata")]++
                          (if version>=llvm3_2
                           then [(memberFun { ftReturnType = normalT $ ref $ NamedType llvmNS "iplist"
                                                             [normalT $ llvmType "NamedMDNode"] False
                                            , ftName = "getNamedMDList"
                                            },"moduleGetNamedMDList")]
                           else [])++
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                      , ftName = "getFunction"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      },"moduleGetFunction")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                      , ftName = "getOrInsertFunction"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")
                                                 ,(False,normalT $ ptr $ llvmType "FunctionType")
                                                 ,if version>=llvm3_3
                                                  then (False,normalT $ llvmType "AttributeSet")
                                                  else (False,normalT $ llvmType "AttrListPtr")]
                                      },"moduleGetOrInsertFunction")
                          ]
             }
       ,Spec { specHeader = if version>=llvm3_3
                            then "llvm/IRReader/IRReader.h"
                            else "llvm/Support/IRReader.h"
             , specNS = []
             , specName = "llvm"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT (ptr $ llvmType "Module") 
                                      , ftName = "ParseIR"
                                      , ftArgs = [(False,normalT (ptr $ llvmType "MemoryBuffer"))
                                                 ,(False,normalT (ref $ llvmType "SMDiagnostic"))
                                                 ,(False,normalT (ref $ llvmType "LLVMContext"))]
                                      , ftStatic = True
                                      },"parseIR")
                          ]++
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isa"
                                      , ftTemplArgs = [to_tp,from_tp]
                                      , ftArgs = [(True,toConstRef from_tp)]
                                      , ftStatic = True
                                      , ftOverloaded = True
                                      , ftPure = True
                                      },"isA"++to)
                           | (to,from) <- [(to',"Value")
                                              | to' <- ["Value"
                                                      ,"Argument"
                                                      ,"BasicBlock"
                                                      ,"InlineAsm"]++
                                                      (if version<llvm3_6
                                                       then ["MDNode"
                                                            ,"MDString"]
                                                       else [])++
                                                      (if version<llvm3_5
                                                       then ["PseudoSourceValue"
                                                            ,"FixedStackPseudoSourceValue"]
                                                       else [])++
                                                      ["User"
                                                      ,"Constant"
                                                      ,"BlockAddress"
                                                      ,"ConstantAggregateZero"
                                                      ,"ConstantArray"]++
                                                      (if version>=llvm3_1
                                                       then ["ConstantDataSequential"
                                                            ,"ConstantDataArray"
                                                            ,"ConstantDataVector"]
                                                       else [])++
                                                      ["ConstantExpr"
                                                      {-,"BinaryConstantExpr"
                                                        ,"CompareConstantExpr"
                                                        ,"ExtractElementConstantExpr"
                                                        ,"ExtractValueConstantExpr"
                                                        ,"GetElementPtrConstantExpr"
                                                        ,"InsertElementConstantExpr"
                                                        ,"InsertValueConstantExpr"
                                                        ,"SelectConstantExpr"
                                                        ,"ShuffleVectorConstantExpr"
                                                        ,"UnaryConstantExpr"-}
                                                      ,"ConstantFP"
                                                      ,"ConstantInt"
                                                      ,"ConstantPointerNull"
                                                      ,"ConstantStruct"
                                                      ,"ConstantVector"
                                                      ,"GlobalValue"
                                                      ,"Function"
                                                      ,"GlobalAlias"
                                                      ,"GlobalVariable"
                                                      ,"UndefValue"
                                                      ,"Instruction"]++
                                                      (if version>=llvm3_0
                                                       then ["AtomicCmpXchgInst"
                                                            ,"AtomicRMWInst"]
                                                       else [])++
                                                      ["BinaryOperator"
                                                      ,"CallInst"
                                                      ,"CmpInst"
                                                      ,"FCmpInst"
                                                      ,"ICmpInst"
                                                      ,"ExtractElementInst"]++
                                                      (if version>=llvm3_0
                                                       then ["FenceInst"]
                                                       else [])++
                                                      ["GetElementPtrInst"
                                                      ,"InsertElementInst"
                                                      ,"InsertValueInst"]++
                                                      (if version>=llvm3_0
                                                       then ["LandingPadInst"]
                                                       else [])++
                                                      ["PHINode"
                                                      ,"SelectInst"
                                                      ,"ShuffleVectorInst"
                                                      ,"StoreInst"
                                                      ,"TerminatorInst"
                                                      ,"BranchInst"
                                                      ,"IndirectBrInst"
                                                      ,"InvokeInst"]++
                                                      (if version>=llvm3_0
                                                       then ["ResumeInst"]
                                                       else [])++
                                                      ["ReturnInst"
                                                      ,"SwitchInst"
                                                      ,"UnreachableInst"
                                                      ,"UnaryInstruction"
                                                      ,"AllocaInst"
                                                      ,"CastInst"
                                                      ,"BitCastInst"]++
                                                      (if version>=llvm3_4
                                                       then ["AddrSpaceCastInst"]
                                                       else [])++
                                                      ["FPExtInst"
                                                      ,"FPToSIInst"
                                                      ,"FPToUIInst"
                                                      ,"FPTruncInst"
                                                      ,"IntToPtrInst"
                                                      ,"PtrToIntInst"
                                                      ,"SExtInst"
                                                      ,"SIToFPInst"
                                                      ,"TruncInst"
                                                      ,"UIToFPInst"
                                                      ,"ZExtInst"
                                                      ,"ExtractValueInst"
                                                      ,"LoadInst"
                                                      ,"VAArgInst"
                                                      ,"Operator"
                                                      ]]++
                                         [(to',"Type") 
                                              | to' <- ["Type"
                                                      ,"CompositeType"
                                                      ,"SequentialType"
                                                      ,"ArrayType"
                                                      ,"PointerType"
                                                      ,"VectorType"
                                                      ,"StructType"
                                                      ,"FunctionType"
                                                      ,"IntegerType"]]++
                                         (if version>=llvm3_6
                                          then [(to',"Metadata")
                                               | to' <- ["MDNode"
                                                        ,"MDString"]]
                                          else [])++
                                         (if version<llvm3_5
                                          then []
                                          else [("FixedStackPseudoSourceValue","PseudoSourceValue")])
                          , let to_tp = normalT $ NamedType llvmNS to [] False
                                from_tp = normalT $ NamedType llvmNS from [] False
                          ]
             }
       ,Spec { specHeader = "llvm/CodeGen/PseudoSourceValue.h"
             , specNS = llvmNS
             , specName = "PseudoSourceValue"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantAggregateZero"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = irInclude version "Instruction.h"
             , specNS = llvmNS
             , specName = "Instruction"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getParent"
                                      , ftOverloaded = True
                                      },"instructionGetParent_")
                          ,(memberFun { ftReturnType = constT $ ref $ llvmType "DebugLoc"
                                      , ftName = "getDebugLoc"
                                      , ftOverloaded = True
                                      },"instructionGetDebugLoc_")
                          ,(memberFun { ftReturnType = constT $ ptr $ llvmType "MDNode"
                                      , ftName = "getMetadata"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },"instructionGetMetadataById_")
                          ,(memberFun { ftReturnType = constT $ ptr $ llvmType "MDNode"
                                      , ftName = "getMetadata"
                                      , ftArgs = [(False,if version >= llvm3_1
                                                         then normalT $ llvmType "StringRef"
                                                         else constT $ ptr char)]
                                      , ftOverloaded = True
                                      },"instructionGetMetadataByName_")
                          ,(memberFun { ftName = "getAllMetadata"
                                      , ftArgs = [(False,normalT $ ref $ NamedType llvmNS "SmallVector"
                                                            [normalT $ NamedType [ClassName "std" []] "pair" [normalT unsigned
                                                                                                             ,normalT $ ptr $ llvmType "MDNode"] False
                                                            ,TypeInt 16] False)]
                                      , ftOverloaded = True
                                      },"instructionGetAllMetadata_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isUsedOutsideOfBlock"
                                      , ftArgs = [(False,constT $ ptr $ llvmType "BasicBlock")]
                                      , ftOverloaded = True
                                      },"instructionIsUsedOutsideOfBlock_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getOpcode"
                                      , ftOverloaded = True
                                      },"instructionGetOpcode_")
                          ,(memberFun { ftReturnType = constT $ ptr char
                                      , ftName = "getOpcodeName"
                                      , ftOverloaded = True
                                      },"instructionGetOpcodeName_")
                          ,(memberFun { ftName = "insertBefore"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Instruction")]
                                      , ftOverloaded = True
                                      },"instructionInsertBefore_")
                          ,(memberFun { ftName = "insertAfter"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Instruction")]
                                      , ftOverloaded = True
                                      },"instructionInsertAfter_")
                          ,(memberFun { ftName = "moveBefore"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Instruction")]
                                      , ftOverloaded = True
                                      },"instructionMoveBefore_")
                          ,(memberFun { ftName = "removeFromParent"
                                      , ftOverloaded = True
                                      },"instructionRemoveFromParent_")]
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "AtomicCmpXchgInst"
                , specTemplateArgs = []
                , specType = classSpec $
                             [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isVolatile"
                                         },"atomicCmpXchgInstIsVolatile")]++
                             (if version<llvm3_5
                              then [(memberFun { ftReturnType = normalT int
                                               , ftName = "getOrdering"
                                               },"atomicCmpXchgInstGetOrdering_")]
                              else [(memberFun { ftReturnType = normalT int
                                               , ftName = "getSuccessOrdering"
                                               },"atomicCmpXchgInstGetSuccessOrdering_")
                                   ,(memberFun { ftReturnType = normalT int
                                               , ftName = "getFailureOrdering"
                                               },"atomicCmpXchgInstGetFailureOrdering_")])++
                             [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getPointerOperand"
                                         },"atomicCmpXchgInstGetPointerOperand")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getCompareOperand"
                                         },"atomicCmpXchgInstGetCompareOperand")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getNewValOperand"
                                         },"atomicCmpXchgInstGetNewValOperand")
                             ,(Constructor ([(True,normalT $ ptr $ llvmType "Value")
                                            ,(True,normalT $ ptr $ llvmType "Value")
                                            ,(True,normalT $ ptr $ llvmType "Value")
                                            ,(False,normalT $ EnumType llvmNS "AtomicOrdering")]++
                                            (if version<llvm3_5
                                             then []
                                             else [(False,normalT $ EnumType llvmNS "AtomicOrdering")])++
                                           [(False,normalT $ EnumType llvmNS "SynchronizationScope")
                                           ]),"newAtomicCmpXchgInst_")]
                }
          ,Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "AtomicRMWInst"
                , specTemplateArgs = []
                , specType = classSpec 
                             [(memberFun { ftReturnType = normalT int
                                         , ftName = "getOperation"
                                         },"atomicRMWInstGetOperation_")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isVolatile"
                                         },"atomicRMWInstIsVolatile")
                             ,(memberFun { ftReturnType = normalT int
                                         , ftName = "getOrdering"
                                         },"atomicRMWInstGetOrdering_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getPointerOperand"
                                         },"atomicRMWInstGetPointerOperand")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getValOperand"
                                         },"atomicRMWInstGetValOperand")
                             ,(Constructor [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "AtomicRMWInst" []] "BinOp")
                                           ,(True,normalT $ ptr $ llvmType "Value")
                                           ,(True,normalT $ ptr $ llvmType "Value")
                                           ,(False,normalT $ EnumType llvmNS "AtomicOrdering")
                                           ,(False,normalT $ EnumType llvmNS "SynchronizationScope")
                                           ],"newAtomicRMWInst_")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "BinaryOperator"
             , specTemplateArgs = []
             , specType = classSpec 
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getOpcode"
                                      },"binOpGetOpCode_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BinaryOperator"
                                      , ftName = "Create"
                                      , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "Instruction" []] "BinaryOps")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newBinaryOperator_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "hasNoUnsignedWrap"
                                      },"binOpHasNoUnsignedWrap")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "hasNoSignedWrap"
                                      },"binOpHasNoSignedWrap")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isExact"
                                      },"binOpIsExact")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "CallInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isTailCall"
                                      },"callInstIsTailCall")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumArgOperands"
                                      },"callInstGetNumArgOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getArgOperand"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"callInstGetArgOperand_")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "setArgOperand"
                                      , ftArgs = [(False,normalT unsigned)
                                                 ,(True,normalT $ ptr $ llvmType "Value")]
                                      },"callInstSetArgOperand_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getCallingConv"
                                      },"callInstGetCallingConv_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCalledValue"
                                      },"callInstGetCalledValue")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "setCalledFunction"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")]
                                      },"callInstSetCalledFunction_")
                          {-,(memberFun { ftReturnType = normalT void
                                      , ftName = "mutateFunctionType"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "FunctionType")]
                                      },"callInstMutateFunctionType")-}
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "CallInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,if version>=llvm3_0
                                                  then (False,normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Value"] False)
                                                  else (False,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newCallInst_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "CmpInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getPredicate" 
                                      , ftOverloaded = True
                                      },"cmpInstGetPredicate_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getInversePredicate"
                                      , ftArgs = [(False,normalT $
                                                         EnumType [ClassName "llvm" []
                                                                  ,ClassName "CmpInst" []]
                                                         "Predicate")]
                                      , ftStatic = True
                                      , ftPure = True },"cmpInstGetInversePredicate_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getSwappedPredicate"
                                      , ftArgs = [(False,normalT $
                                                         EnumType [ClassName "llvm" []
                                                                  ,ClassName "CmpInst" []]
                                                         "Predicate")]
                                      , ftStatic = True
                                      , ftPure = True },"cmpInstGetSwappedPredicate_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FCmpInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor
                            [(False,normalT $ EnumType [ClassName "llvm" []
                                                       ,ClassName "CmpInst" []] "Predicate")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(False,constT $ ref $ llvmType "Twine")
                            ],"newFCmpInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ICmpInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor
                            [(False,normalT $ EnumType [ClassName "llvm" []
                                                       ,ClassName "CmpInst" []] "Predicate")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(False,constT $ ref $ llvmType "Twine")
                            ],"newICmpInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ExtractElementInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getVectorOperand"
                                      },"extractElementInstGetVectorOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getIndexOperand"
                                      },"extractElementInstGetIndexOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ExtractElementInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newExtractElementInst_")]
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "FenceInst"
                , specTemplateArgs = []
                , specType = classSpec 
                             [(memberFun { ftReturnType = normalT int
                                         , ftName = "getOrdering"
                                         },"fenceInstGetOrdering_")
                             ,(Constructor
                               [(False,normalT $ ref $ llvmType "LLVMContext")
                               ,(False,normalT $ EnumType llvmNS "AtomicOrdering")
                               ,(False,normalT $ EnumType llvmNS "SynchronizationScope")
                               ],"newFenceInst_")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "GetElementPtrInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "PointerType"
                                      , ftName = "getType"
                                      },"getElementPtrInstGetType")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isInBounds"
                                      },"getElementPtrInstIsInBounds")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getPointerOperand"
                                      },"getElementPtrInstGetPointerOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                      , ftName = "idx_begin"
                                      },"getElementPtrInstIdxBegin")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                      , ftName = "idx_end"
                                      },"getElementPtrInstIdxEnd")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumIndices"
                                      },"getElementPtrInstGetNumIndices_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "GetElementPtrInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,if version>=llvm3_0
                                                  then (False,normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Value"] False)
                                                  else (False,normalT $ ptr $ llvmType "Value")
                                                 ,(False,normalT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newGetElementPtrInst_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "InsertElementInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "VectorType"
                                      , ftName = "getType"
                                      },"insertElementInstGetType")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "InsertElementInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newInsertElementInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "InsertValueInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "InsertValueInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,if version>=llvm3_0
                                                  then (False,normalT $ NamedType llvmNS "ArrayRef" [normalT unsigned] False)
                                                  else (False,normalT unsigned)
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newInsertValueInst_")]
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "LandingPadInst"
                , specTemplateArgs = []
                , specType = classSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getPersonalityFn"
                                         },"landingPadInstGetPersonaliteFn")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isCleanup"
                                         },"landingPadInstIsCleanup")
                             ,(memberFun { ftReturnType = normalT unsigned
                                         , ftName = "getNumClauses"
                                         },"landingPadInstGetNumClauses_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getClause"
                                         , ftArgs = [(False,normalT unsigned)]
                                         },"landingPadInstGetClause_")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isCatch"
                                         , ftArgs = [(False,normalT unsigned)]
                                         },"landingPadInstIsCatch_")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isFilter"
                                         , ftArgs = [(False,normalT unsigned)]
                                         },"landingPadInstIsFilter_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "LandingPadInst"
                                         , ftName = "Create"
                                         , ftArgs = [(True,normalT $ ptr $ llvmType "Type")
                                                    ,(True,normalT $ ptr $ llvmType "Value")
                                                    ,(False,normalT unsigned)
                                                    ,(False,constT $ ref $ llvmType "Twine")]
                                         , ftStatic = True
                                         },"newLandingPadInst_")
                             ,(memberFun { ftName = "setCleanup"
                                         , ftArgs = [(False,normalT bool)]
                                         },"landingPadInstSetCleanup")
                             ,(memberFun { ftName = "addClause"
                                         , ftArgs = [(True,normalT $ ptr $ llvmType (if version<llvm3_5
                                                                                     then "Value"
                                                                                     else "Constant"))]
                                         },"landingPadInstAddClause_")
                             ]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "PHINode"
             , specTemplateArgs = []
             , specType = classSpec 
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumIncomingValues"
                                      },"phiNodeGetNumIncomingValues_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getIncomingValue"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"phiNodeGetIncomingValue_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getIncomingBlock"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"phiNodeGetIncomingBlock_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "PHINode"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]++
                                                 (if version>=llvm3_0
                                                  then [(False,normalT unsigned)]
                                                  else [])++
                                                 [(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newPhiNode_")
                          ,(memberFun { ftName = "addIncoming"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,normalT $ ptr $ llvmType "BasicBlock")]
                                      },"phiNodeAddIncoming_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SelectInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCondition"
                                      },"selectInstGetCondition")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getTrueValue"
                                      },"selectInstGetTrueValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getFalseValue"
                                      },"selectInstGetFalseValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "SelectInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newSelectInst_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ShuffleVectorInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "VectorType"
                                      , ftName = "getType"
                                      },"shuffleVectorInstGetType")
                          ,(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Value")
                                        ,(False,constT $ ref $ llvmType "Twine")
                                        ],"newShuffleVectorInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "StoreInst"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },"storeInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },"storeInstGetAlignment_")
                          ,(Constructor $ [(True,normalT $ ptr $ llvmType "Value")
                                          ,(True,normalT $ ptr $ llvmType "Value")
                                          ,(False,normalT bool)
                                          ,(False,normalT unsigned)]++
                            (if version>=llvm3_0
                             then [(False,normalT $ EnumType llvmNS "AtomicOrdering")
                                  ,(False,normalT $ EnumType llvmNS "SynchronizationScope")]
                             else []),"newStoreInst_")]++
               (if version>=llvm3_0
                then [(memberFun { ftReturnType = normalT int
                                 , ftName = "getOrdering"
                                 },"storeInstGetOrdering_")]
                else [])++
               [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                           , ftName = "getValueOperand"
                           },"storeInstGetValueOperand")
               ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                           , ftName = "getPointerOperand"
                           },"storeInstGetPointerOperand")
               ]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "TerminatorInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumSuccessors"
                                      , ftOverloaded = True
                                      },"terminatorInstGetNumSuccessors_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getSuccessor"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },"terminatorInstGetSuccessor_")]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "BranchInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isConditional"
                                      },"branchInstIsConditional")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCondition"
                                      },"branchInstGetCondition")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BranchInst"
                                      , ftName = "Create"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "BasicBlock")]
                                      , ftStatic = True
                                      },"newBranchInst")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BranchInst"
                                      , ftName = "Create"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "BasicBlock")
                                                 ,(False,normalT $ ptr $ llvmType "BasicBlock")
                                                 ,(True,normalT $ ptr $ llvmType "Value")]
                                      , ftStatic = True
                                      },"newBranchInstCond_")]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "IndirectBrInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getAddress"
                                      },"indirectBrInstGetAddress")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumDestinations"
                                      },"indirectBrInstGetNumDestinations")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getDestination"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"indirectBrInstGetDestination")
                          ,(memberFun { ftName = "addDestination"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "BasicBlock")]
                                      },"indirectBrInstAddDestination")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "IndirectBrInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },"newIndirectBrInst_")]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "InvokeInst"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumArgOperands"
                                      },"invokeInstGetNumArgOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftName = "getArgOperand"
                                      },"invokeInstGetArgOperand_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getCallingConv"
                                      },"invokeInstGetCallingConv_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCalledValue"
                                      },"invokeInstGetCalledValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getNormalDest"
                                      },"invokeInstGetNormalDest")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getUnwindDest"
                                      },"invokeInstGetUnwindDest")
                          ,(memberFun { ftReturnType = constT $ ref $ llvmType $ if version>=llvm3_3
                                                                                 then "AttributeSet"
                                                                                 else "AttrListPtr"
                                      , ftName = "getAttributes"
                                      },"invokeInstGetAttributes")]++
                          (if version>=llvm3_0
                           then [(memberFun { ftReturnType = normalT $ ptr $ llvmType "LandingPadInst"
                                            , ftName = "getLandingPadInst"
                                            },"invokeInstGetLandingPadInst")]
                           else [])++
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "InvokeInst"
                                      , ftName = "Create"
                                      , ftArgs = if version>=llvm3_0
                                                 then [(True,normalT $ ptr $ llvmType "Value")
                                                      ,(False,normalT $ ptr $ llvmType "BasicBlock")
                                                      ,(False,normalT $ ptr $ llvmType "BasicBlock")
                                                      ,(False,normalT $ NamedType llvmNS "ArrayRef"
                                                              [normalT $ ptr $ llvmType "Value"] False)
                                                      ,(False,normalT $ ref $ llvmType "Twine")]
                                                 else [(True,normalT $ ptr $ llvmType "Value")
                                                      ,(False,normalT $ ptr $ llvmType "BasicBlock")
                                                      ,(False,normalT $ ptr $ llvmType "BasicBlock")
                                                      ,(False,normalT $ NamedType
                                                              [ClassName "std" []
                                                              ,ClassName "vector"
                                                               [normalT $ ptr $ llvmType "Value"]]
                                                              "const_iterator" [] False)
                                                      ,(False,normalT $ NamedType
                                                              [ClassName "std" []
                                                              ,ClassName "vector"
                                                               [normalT $ ptr $ llvmType "Value"]]
                                                              "const_iterator" [] False)
                                                      ,(False,normalT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newInvokeInst_")]
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "InstrTypes.h"
                , specNS = llvmNS
                , specName = "ResumeInst"
                , specTemplateArgs = []
                , specType = classSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getValue"
                                         },"resumeInstGetValue")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ResumeInst"
                                         , ftName = "Create"
                                         , ftArgs = [(True,normalT $ ptr $ llvmType "Value")]
                                         , ftStatic = True
                                         },"newResumeInst_")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "ReturnInst"
             , specTemplateArgs = []
             , specType = classSpec 
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getReturnValue"
                                      },"returnInstGetReturnValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ReturnInst"
                                      , ftName = "Create"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(True,normalT $ ptr $ llvmType "Value")]
                                      , ftStatic = True
                                      },"newReturnInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SwitchInst"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCondition"
                                      },"switchInstGetCondition")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getDefaultDest"
                                      },"switchInstGetDefaultDest")
                          ,(memberFun { ftName = "addCase"
                                      , ftArgs = [(False,normalT $ ptr $ llvmType "ConstantInt")
                                                 ,(False,normalT $ ptr $ llvmType "BasicBlock")]
                                      },"switchInstAddCase")]++
                          (if version>=llvm3_1
                           then [(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" [] False
                                            , ftName = "case_begin"
                                            },"switchInstCaseBegin")
                                ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" [] False
                                            , ftName = "case_end"
                                            },"switchInstCaseEnd")
                                ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" [] False
                                            , ftName = "case_default"
                                            },"switchInstCaseDefault")]
                           else [(memberFun { ftReturnType = normalT $ ptr $ llvmType "ConstantInt"
                                            , ftName = "getCaseValue"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"switchInstGetCaseValue")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getNumCases"
                                            },"switchInstGetNumCases")])++
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "SwitchInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,normalT $ ptr $ llvmType "BasicBlock")
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },"newSwitchInst_")]
             }]++
    (if version>=llvm3_1
     then [Spec { specHeader = irInclude version "InstrTypes.h"
                , specNS = [ClassName "llvm" [],ClassName "SwitchInst" []]
                , specName = "CaseIt"
                , specTemplateArgs = []
                , specType = classSpec
                             [(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" [] False
                                         , ftName = "operator++"
                                         },"caseItNext")
                             ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" [] False
                                         , ftName = "operator--"
                                         },"caseItPrev")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "operator=="
                                         , ftArgs = [(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" [] False)]
                                         },"caseItEq")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ConstantInt"
                                         , ftName = "getCaseValue"
                                         },"caseItGetCaseValue")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                         , ftName = "getCaseSuccessor"
                                         },"caseItGetCaseSuccessor")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "UnreachableInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(False,normalT $ ref $ llvmType "LLVMContext")],"newUnreachableInst")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "UnaryInstruction"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "AllocaInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "PointerType"
                                      , ftName = "getType"
                                      },"allocaInstGetType")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isArrayAllocation"
                                      },"allocaInstIsArrayAllocation")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getArraySize"
                                      },"allocaInstGetArraySize")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },"allocaInstGetAlignment_")
                          ,(Constructor [(True,normalT $ ptr $ llvmType "Type")
                                        ,(True,normalT $ ptr $ llvmType "Value")
                                        ,(False,normalT unsigned)
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newAllocaInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "CastInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getOpcode"
                                      , ftOverloaded = True
                                      },"castInstGetOpcode_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "BitCastInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newBitCastInst_")]
             }]++
       (if version>=llvm3_4
        then [Spec { specHeader = irInclude version "Instructions.h"
                   , specNS = llvmNS
                   , specName = "AddrSpaceCastInst"
                   , specTemplateArgs = []
                   , specType = classSpec
                                [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                              ,(True,normalT $ ptr $ llvmType "Type")
                                              ,(False,normalT $ ref $ llvmType "Twine")
                                              ],"newAddrSpaceCastInst_")]
                   }]
        else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FPExtInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newFPExtInstInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FPToSIInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newFPToSIInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FPToUIInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newFPToUIInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FPTruncInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newFPTruncInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "IntToPtrInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newIntToPtrInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "PtrToIntInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newPtrToIntInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SExtInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newSExtInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SIToFPInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newSIToFPInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "TruncInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newTruncInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "UIToFPInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newUIToFPInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ZExtInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,normalT $ ref $ llvmType "Twine")
                                        ],"newZExtInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ExtractValueInst"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "ExtractValueInst"
                                      , ftName = "Create"
                                      , ftArgs = if version>=llvm3_0
                                                 then [(True,normalT $ ptr $ llvmType "Value")
                                                      ,(False,normalT $ NamedType llvmNS "ArrayRef" [normalT unsigned] False)
                                                      ,(False,constT $ ref $ llvmType "Twine")]
                                                 else [(True,normalT $ ptr $ llvmType "Value")
                                                      ,(False,normalT $ NamedType
                                                              [ClassName "std" []
                                                              ,ClassName "vector"
                                                               [normalT unsigned]]
                                                              "const_iterator" [] False)
                                                      ,(False,normalT $ NamedType
                                                              [ClassName "std" []
                                                              ,ClassName "vector"
                                                               [normalT unsigned]]
                                                              "const_iterator" [] False)
                                                      ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },"newExtractValueInst_")
                          ,(memberFun { ftReturnType = constT $ ptr unsigned
                                      , ftName = "idx_begin"
                                      },"extractValueInstIdxBegin")
                          ,(memberFun { ftReturnType = constT $ ptr unsigned
                                      , ftName = "idx_end"
                                      },"extractValueInstIdxEnd")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumIndices"
                                      },"extractValueInstGetNumIndices")]++
                          (if version>=llvm3_0
                           then [(memberFun { ftReturnType = normalT $ NamedType llvmNS "ArrayRef" [normalT unsigned] False
                                            , ftName = "getIndices"
                                            },"extractValueInstGetIndices")]
                           else [])
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "LoadInst"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },"loadInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },"loadInstGetAlignment_")]++
               (if version>=llvm3_0
                then [(memberFun { ftReturnType = normalT int
                                 , ftName = "getOrdering"
                                 },"loadInstGetOrdering_")]
                else [])++
               [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                           , ftName = "getPointerOperand"
                           },"loadInstGetPointerOperand")
               ,(Constructor $ [(True,normalT $ ptr $ llvmType "Value")
                               ,(False,constT $ ref $ llvmType "Twine")
                               ,(False,normalT bool)
                               ,(False,normalT unsigned)]++
                 (if version>=llvm3_0
                  then [(False,normalT $ EnumType llvmNS "AtomicOrdering")
                       ,(False,normalT $ EnumType llvmNS "SynchronizationScope")]
                  else []),"newLoadInst_")
               ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "VAArgInst"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                        ,(True,normalT $ ptr $ llvmType "Type")
                                        ,(False,constT $ ref $ llvmType "Twine")
                                        ],"newVAArgInst_")]
             }
       ,Spec { specHeader = irInclude version "User.h"
             , specNS = llvmNS
             , specName = "User"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumOperands" 
                                      , ftOverloaded = True
                                      },"getNumOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getOperand" 
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },"getOperand_")
                          ,(memberFun { ftReturnType = normalT $ ref $ llvmType "Use"
                                      , ftName = "getOperandUse"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },"getOperandUse_")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "replaceUsesOfWith"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")]
                                      , ftOverloaded = True
                                      },"replaceUsesOfWith_")]
             }
       ,Spec { specHeader = irInclude version "Operator.h"
             , specNS = llvmNS
             , specName = "Operator"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = irInclude version "Use.h"
             , specNS = llvmNS
             , specName = "Use"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "get"
                                      },"useGet")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                      , ftName = "getNext"
                                      },"useGetNext")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "User"
                                      , ftName = "getUser"
                                      },"useGetUser")
                          ,(memberFun { ftName = "set"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")]
                                      },"useSet_")]
               
             }]++
       [Spec { specHeader = if version<llvm3_5
                            then irInclude version "Use.h"
                            else "llvm/IR/Value.h"
             , specNS = if version<llvm3_5
                        then llvmNS
                        else [ClassName "llvm" []
                             ,ClassName "Value" []]
             , specName = if version<llvm3_5
                          then "value_use_iterator"
                          else "use_iterator"
             , specTemplateArgs = if version<llvm3_5
                                  then [rtp]
                                  else []
             , specType = classSpec $
                          [(memberFun { ftReturnType = toPtr rtp
                                      , ftName = "operator->"
                                      },"valueUseIterator"++tp++"Deref")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "operator=="
                                      , ftArgs = [(False,constT $ ref self)]
                                      },"valueUseIterator"++tp++"Eq")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "operator!="
                                      , ftArgs = [(False,constT $ ref self)]
                                      },"valueUseIterator"++tp++"NEq")
                          ,(memberFun { ftReturnType = normalT $ ref self
                                      , ftName = "operator++"
                                      , ftIgnoreReturn = True
                                      },"valueUseIterator"++tp++"Next")]++
                          (if version<llvm3_5
                           then [(memberFun { ftReturnType = normalT $ ref $ llvmType "Use"
                                            , ftName = "getUse"
                                            },"valueUseIterator"++tp++"GetUse")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getOperandNo"
                                            },"valueUseIterator"++tp++"GetOperandNo")]
                           else [])
             }
       | tp <- if version<llvm3_5
               then ["User"]
               else ["Use"]
       , let rtp = Type [] (llvmType tp)
       , let self = if version<llvm3_5
                    then NamedType llvmNS
                         "value_use_iterator" [rtp] False
                    else NamedType [ClassName "llvm" []
                                   ,ClassName "Value" []]
                         "use_iterator" [] False ]++
       [Spec { specHeader = "llvm/PassManager.h"
             , specNS = llvmNS
             , specName = "PassManager"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [],"newPassManager")
                          ,(Destructor False,"deletePassManager")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "add"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Pass")]
                                      },"passManagerAdd_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "run"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Module")]
                                      },"passManagerRun")
                          ]
             }
       ,Spec { specHeader = "llvm/PassManager.h"
             , specNS = llvmNS
             , specName = "FunctionPassManager"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(False,normalT $ ptr $ llvmType "Module")],"newFunctionPassManager")
                          ,(Destructor False,"deleteFunctionPassManager")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "add"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Pass")]
                                      },"functionPassManagerAdd_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "run"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Function")]
                                      },"functionPassManagerRun")
                          ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "Pass"
             , specTemplateArgs = []
             , specType = classSpec 
                          [(Destructor True,"deletePass_")
                          ,(memberFun { ftReturnType = constT $ ptr $ llvmType "PassInfo"
                                      , ftName = "lookupPassInfo"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      , ftStatic = True
                                      },"passLookupPassInfo")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "AnalysisResolver"
                                      , ftName = "getResolver"
                                      , ftOverloaded = True
                                      },"passGetResolver_")
                          ,(memberFun { ftReturnType = normalT $ ptr void
                                      , ftName = "getAdjustedAnalysisPointer"
                                      , ftArgs = [(False,constT $ ptr void)]
                                      , ftOverloaded = True
                                      },"passGetAdjustedAnalysisPointer_")
                          ,(memberFun { ftReturnType = normalT $ EnumType llvmNS "PassKind"
                                      , ftName = "getPassKind"
                                      , ftOverloaded = True
                                      },"passGetKind_")
                          ,(memberFun { ftReturnType = constT $ ptr char
                                      , ftName = "getPassName"
                                      , ftOverloaded = True
                                      },"passGetName_")
                          ,(memberFun { ftName = "dump"
                                      , ftOverloaded = True
                                      },"passDump_")
                          ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "FunctionPass"
             , specTemplateArgs = []
             , specType = classSpec 
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "runOnFunction"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Function")]
                                      , ftOverloaded = True
                                      },"functionPassRun_")]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "ModulePass"
             , specTemplateArgs = []
             , specType = classSpec 
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "runOnModule"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Module")]
                                      , ftOverloaded = True
                                      },"modulePassRunOnModule_")
                          ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "ImmutablePass"
             , specTemplateArgs = []
             , specType = classSpec []
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "BasicBlockPass"
             , specTemplateArgs = []
             , specType = classSpec []
             }]++
       (if version<llvm3_6
        then [Spec { specHeader = "llvm/Analysis/FindUsedTypes.h"
                   , specNS = llvmNS
                   , specName = "FindUsedTypes"
                   , specTemplateArgs = []
                   , specType = classSpec 
                                [(Constructor [],"newFindUsedTypes")
                                ,(Destructor False,"deleteFindUsedTypes")
                                ,(memberFun { ftReturnType = constT $ ref $ NamedType [ClassName "llvm" []] "SetVector" [normalT $ ptr $ llvmType "Type"] False
                                            , ftName = "getTypes"
                                            },"findUsedTypesGetTypes")]
                   }]
        else [])++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/Target/TargetLibraryInfo.h"
                , specNS = llvmNS
                , specName = "TargetLibraryInfo"
                , specTemplateArgs = []
                , specType = classSpec $
                             [(Constructor [],"newTargetLibraryInfo")
                             ,(Destructor False,"deleteTargetLibraryInfo")]++
                             (if version>=llvm3_3
                              then [(memberFun { ftReturnType = normalT bool
                                               , ftName = "getLibFunc"
                                               , ftArgs = [(False,normalT $ llvmType "StringRef")
                                                          ,(False,normalT $ ref $ EnumType [ClassName "llvm" [],ClassName "LibFunc" []] "Func")]
                                               },"targetLibraryInfoGetLibFunc_")]
                              else [])++
                             (if version>=llvm3_1
                              then [(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                               , ftName = "getName"
                                               , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "LibFunc" []] "Func")]
                                               },"targetLibraryInfoGetName_")]
                              else [])++
                             [(memberFun { ftReturnType = normalT bool
                                         , ftName = "has"
                                         , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "LibFunc" []] "Func")]
                                         },"targetLibraryInfoHas_")
                             ]
                }]
     else [])++
       (if version >= llvm3_2
        then [Spec { specHeader = irInclude version "DataLayout.h"
                   , specNS = llvmNS
                   , specName = "DataLayout"
                   , specTemplateArgs = []
                   , specType = classSpec $
                                [(Constructor [(False,normalT $ llvmType "StringRef")],"newDataLayoutFromString")
                                ,(Constructor [(False,constT $ ptr $ llvmType "Module")],"newDataLayoutFromModule")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isLittleEndian"
                                            },"dataLayoutIsLittleEndian")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isBigEndian"
                                            },"dataLayoutIsBigEndian")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isLegalInteger"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"dataLayoutIsLegalInteger")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "exceedsNaturalStackAlignment"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"dataLayoutExceedsNaturalStackAlignment")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "fitsInLegalInteger"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"dataLayoutFitsInLegalInteger")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPointerABIAlignment"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"dataLayoutPointerABIAlignment")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPointerPrefAlignment"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"dataLayoutPointerPrefAlignment")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPointerSize"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"dataLayoutPointerSize")
                                ,(memberFun { ftReturnType = normalT uint64_t
                                            , ftName = "getTypeSizeInBits"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"dataLayoutTypeSizeInBits_")
                                ,(memberFun { ftReturnType = normalT uint64_t
                                            , ftName = "getTypeStoreSize"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"dataLayoutTypeStoreSize_")
                                ,(memberFun { ftReturnType = normalT uint64_t
                                            , ftName = "getTypeAllocSize"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"dataLayoutTypeAllocSize_")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getABITypeAlignment"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"dataLayoutABITypeAlignment_")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getABIIntegerTypeAlignment"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"dataLayoutABIIntegerTypeAlignment")]++
                                (if version<llvm3_5
                                 then [(memberFun { ftReturnType = normalT unsigned
                                                  , ftName = "getCallFrameTypeAlignment"
                                                  , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                                  },"dataLayoutCallFrameTypeAlignment_")]
                                 else [])++
                                [(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPrefTypeAlignment"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"dataLayoutPrefTypeAlignment_")
                                ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "IntegerType"
                                            , ftName = "getIntPtrType"
                                            , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                       ,(False,normalT unsigned)]
                                            },"dataLayoutIntPtrType")
                                ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Type"
                                            , ftName = "getIntPtrType"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"dataLayoutIntPtrTypeForType_")
                                ,(memberFun { ftReturnType = constT $ ptr $ llvmType "StructLayout"
                                            , ftName = "getStructLayout"
                                            , ftArgs = [(False,normalT $ ptr $ llvmType "StructType")]
                                            },"dataLayoutStructLayout")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPreferredAlignment"
                                            , ftArgs = [(False,normalT $ ptr $ llvmType "GlobalVariable")]
                                            },"dataLayoutPreferedAlignment")]
                   }]
        else [Spec { specHeader = "llvm/Target/TargetData.h"
                   , specNS = llvmNS
                   , specName = "TargetData"
                   , specTemplateArgs = []
                   , specType = classSpec $
                                [(Constructor [(False,normalT $ llvmType "StringRef")],"newTargetDataFromString")
                                ,(Constructor [(False,constT $ ptr $ llvmType "Module")],"newTargetDataFromModule")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isLittleEndian"
                                            },"targetDataIsLittleEndian")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isBigEndian"
                                            },"targetDataIsBigEndian")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isLegalInteger"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"targetDataIsLegalInteger")]++
                                (if version>=llvm3_0
                                 then [(memberFun { ftReturnType = normalT bool
                                                  , ftName = "exceedsNaturalStackAlignment"
                                                  , ftArgs = [(False,normalT unsigned)]
                                                  },"targetDataExceedsNaturalStackAlignment")
                                      ,(memberFun { ftReturnType = normalT bool
                                                  , ftName = "fitsInLegalInteger"
                                                  , ftArgs = [(False,normalT unsigned)]
                                                  },"targetDataFitsInLegalInteger")]
                                 else [])++
                                [(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPointerABIAlignment"
                                            },"targetDataPointerABIAlignment")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPointerPrefAlignment"
                                            },"targetDataPointerPrefAlignment")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPointerSize"
                                            },"targetDataPointerSize")
                                ,(memberFun { ftReturnType = normalT uint64_t
                                            , ftName = "getTypeSizeInBits"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"targetDataTypeSizeInBits_")
                                ,(memberFun { ftReturnType = normalT uint64_t
                                            , ftName = "getTypeStoreSize"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"targetDataTypeStoreSize_")
                                ,(memberFun { ftReturnType = normalT uint64_t
                                            , ftName = "getTypeAllocSize"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"targetDataTypeAllocSize_")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getABITypeAlignment"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"targetDataABITypeAlignment_")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getABIIntegerTypeAlignment"
                                            , ftArgs = [(False,normalT unsigned)]
                                            },"targetDataABIIntegerTypeAlignment")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getCallFrameTypeAlignment"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"targetDataCallFrameTypeAlignment_")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPrefTypeAlignment"
                                            , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                            },"targetDataPrefTypeAlignment_")
                                ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "IntegerType"
                                            , ftName = "getIntPtrType"
                                            , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")]
                                            },"targetDataIntPtrType")
                                ,(memberFun { ftReturnType = constT $ ptr $ llvmType "StructLayout"
                                            , ftName = "getStructLayout"
                                            , ftArgs = [(False,normalT $ ptr $ llvmType "StructType")]
                                            },"targetDataStructLayout")
                                ,(memberFun { ftReturnType = normalT unsigned
                                            , ftName = "getPreferredAlignment"
                                            , ftArgs = [(False,normalT $ ptr $ llvmType "GlobalVariable")]
                                            },"targetDataPreferedAlignment")]
                   }])++
       (if version<llvm3_5
        then []
        else [Spec { specHeader = "llvm/IR/DataLayout.h"
                   , specNS = llvmNS
                   , specName = "DataLayoutPass"
                   , specTemplateArgs = []
                   , specType = classSpec
                                [(memberFun { ftReturnType = constT $ ref $ llvmType "DataLayout"
                                            , ftName = "getDataLayout"
                                            },"dataLayoutPassGetDataLayout")]
                   }])++
       [Spec { specHeader = if version >= llvm3_2
                            then irInclude version "DataLayout.h"
                            else "llvm/Target/TargetData.h"
             , specNS = llvmNS
             , specName = "StructLayout"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getSizeInBytes"
                                      },"structLayoutSizeInBytes")
                          ,(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getSizeInBits"
                                      },"structLayoutSizeInBits")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },"structLayoutAlignment")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getElementContainingOffset"
                                      , ftArgs = [(False,normalT uint64_t)]
                                      },"structLayoutElementContainingOffset")
                          ,(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getElementOffset"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"structLayoutElementOffset")
                          ,(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getElementOffsetInBits"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },"structLayoutElementOffsetInBits")]
             }
       ,Spec { specHeader = "llvm/PassSupport.h"
             , specNS = llvmNS
             , specName = "PassInfo"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Destructor False,"deletePassInfo")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Pass"
                                      , ftName = "createPass"
                                      },"passInfoCreatePass")
                          ,(memberFun { ftReturnType = constT $ ptr $ char
                                      , ftName = "getPassName"
                                      },"passInfoGetPassName_")
                          ,(memberFun { ftReturnType = constT $ ptr $ char
                                      , ftName = "getPassArgument"
                                      },"passInfoGetPassArgument_")
                          ]
             }
       ,Spec { specHeader = "llvm/Analysis/LoopInfo.h"
             , specNS = llvmNS
             , specName = "LoopInfo"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [],"newLoopInfo")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType llvmNS "LoopInfoBase" 
                                                       [normalT $ llvmType "BasicBlock"
                                                       ,normalT $ llvmType "Loop"]
                                                       False
                                      , ftName = "getBase"
                                      },"loopInfoGetBase")
                          ]
             }
       ]++
    (if version>=llvm3_0
     then [Spec { specHeader = "llvm/Transforms/IPO/PassManagerBuilder.h"
                , specNS = llvmNS
                , specName = "PassManagerBuilder"
                , specTemplateArgs = []
                , specType = classSpec $
                             [(Constructor [],"newPassManagerBuilder")
                             ,(Destructor False,"deletePassManagerBuilder")
                             ,(memberFun { ftName = "populateFunctionPassManager"
                                         , ftArgs = [(False,normalT $ ref $ llvmType "FunctionPassManager")
                                                    ]
                                         },"populateFunctionPassManager")
                             ,(Setter { ftSetVar = "OptLevel"
                                      , ftSetType = normalT unsigned
                                      },"setPassManagerBuilderOptLevel")
                             ,(Setter { ftSetVar = "SizeLevel"
                                      , ftSetType = normalT unsigned
                                      },"setPassManagerBuilderSizeLevel")
                             ,(Setter { ftSetVar = "Inliner"
                                      , ftSetType = normalT $ ptr $ llvmType "Pass"
                                      },"setPassManagerBuilderInliner")]++
                             (if version<=llvm3_3
                              then [(Setter { ftSetVar = "DisableSimplifyLibCalls"
                                            , ftSetType = normalT bool
                                            },"setPassManagerBuilderDisableSimplifyLibCalls")]
                              else [])++
                             [(Setter { ftSetVar = "DisableUnitAtATime"
                                      , ftSetType = normalT bool
                                      },"setPassManagerBuilderDisableUnitAtATime")
                             ,(Setter { ftSetVar = "DisableUnrollLoops"
                                      , ftSetType = normalT bool
                                      },"setPassManagerBuilderDisableUnrollLoops")]++
                             (if version>=llvm3_1
                              then (if version>=llvm3_3
                                    then [(Setter { ftSetVar = "BBVectorize"
                                                  , ftSetType = normalT bool
                                                  },"setPassManagerBuilderBBVectorize")
                                         ,(Setter { ftSetVar = "SLPVectorize"
                                                  , ftSetType = normalT bool
                                                  },"setPassManagerBuilderSLPVectorize")]
                                    else [(Setter { ftSetVar = "Vectorize"
                                                  , ftSetType = normalT bool
                                                  },"setPassManagerBuilderVectorize")]++
                                         (if version>=llvm3_2
                                          then [(Setter { ftSetVar = "LoopVectorize"
                                                        , ftSetType = normalT bool
                                                        },"setPassManagerBuilderLoopVectorize")]
                                          else []))
                              else [])
                }]
     else [])++
       [Spec { specHeader = "llvm/Transforms/Scalar.h"
             , specNS = llvmNS
             , specName = f
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType p
                                        , gfunArgs = fmap (\x -> (False,x)) a
                                        , gfunHSName = f
                                        }
             } | (f,p,a) <- [("createConstantPropagationPass","FunctionPass",[])
                            ,("createSCCPPass","FunctionPass",[])
                            ,("createDeadInstEliminationPass","Pass",[])
                            ,("createDeadCodeEliminationPass","FunctionPass",[])
                            ,("createDeadStoreEliminationPass","FunctionPass",[])
                            ,("createAggressiveDCEPass","FunctionPass",[])]++
                            (if version>=llvm3_2
                             then [("createSROAPass","FunctionPass",[normalT bool])]
                             else [])++
                            [("createScalarReplAggregatesPass","FunctionPass",
                              if version>=llvm2_9
                              then [normalT signed
                                   ,normalT bool]++
                                   (if version>=llvm3_2
                                    then [normalT signed
                                         ,normalT signed
                                         ,normalT signed]
                                    else [])
                              else [normalT int])
                            ,("createIndVarSimplifyPass","Pass",[])
                            ,("createInstructionCombiningPass","FunctionPass",[])
                            ,("createLICMPass","Pass",[])
                            ,("createLoopStrengthReducePass","Pass",[])]++
                            (if version>=llvm3_1
                             then [("createGlobalMergePass","Pass",[])]
                             else [])++
                            [("createLoopUnswitchPass","Pass",[normalT bool])]++
                            (if version>=llvm2_9
                             then [("createLoopInstSimplifyPass","Pass",[])]
                             else [])++
                            [("createLoopUnrollPass","Pass",
                              if version>=llvm3_4
                              then [normalT int
                                   ,normalT int
                                   ,normalT int
                                   ,normalT int]
                              else [])]++
                            (if version>=llvm3_5
                             then [("createSimpleLoopUnrollPass","Pass",[])]
                             else [])++
                            (if version>=llvm3_4
                             then [("createLoopRerollPass","Pass",[])]
                             else [])++
                            [("createLoopRotatePass","Pass",[])]++
                            (if version>=llvm2_9
                             then [("createLoopIdiomPass","Pass",[])]
                             else [])++
                            [("createPromoteMemoryToRegisterPass","FunctionPass",[])
                            ,("createDemoteRegisterToMemoryPass","FunctionPass",[])
                            ,("createReassociatePass","FunctionPass",[])
                            ,("createJumpThreadingPass","FunctionPass",[])
                            ,("createCFGSimplificationPass","FunctionPass",[])]++
                            (if version>=llvm3_4
                             then [("createFlattenCFGPass","FunctionPass",[])
                                  ,("createStructurizeCFGPass","Pass",[])]
                             else [])++
                            [("createBreakCriticalEdgesPass","FunctionPass",[])
                            ,("createLoopSimplifyPass","Pass",[])
                            ,("createTailCallEliminationPass","FunctionPass",[])
                            ,("createLowerSwitchPass","FunctionPass",[])
                            ,("createLowerInvokePass","FunctionPass",[])
                            ,("createLCSSAPass","Pass",[])]++
                            (if version>=llvm2_9
                             then [("createEarlyCSEPass","FunctionPass",[])]
                             else [])++
                            {-(if version>=llvm3_5
                             then [("createMergedLoadStoreMotionPass","FunctionPass",[])]
                             else [])++-}
                            [("createGVNPass","FunctionPass",[normalT bool])
                            ,("createMemCpyOptPass","FunctionPass",[])
                            ,("createLoopDeletionPass","Pass",[])]++
                            (if version>=llvm3_5
                             then [("createConstantHoistingPass","FunctionPass",[])]
                             else [])++
                            [("createInstructionNamerPass","FunctionPass",[])
                            ,("createSinkingPass","FunctionPass",[])
                            ,("createLowerAtomicPass","Pass",[])
                            ,("createCorrelatedValuePropagationPass","FunctionPass",[])]++
                            (if version>=llvm2_9
                             then [("createInstructionSimplifierPass","FunctionPass",[])]
                             else [])++
                            (if version>=llvm3_0
                             then [("createLowerExpectIntrinsicPass","FunctionPass",[])]
                             else [])++
                            (if version>=llvm3_4
                             then [("createPartiallyInlineLibCallsPass","FunctionPass",[])
                                  ,("createSampleProfileLoaderPass","FunctionPass",[])]
                             else [])++
                            (if version>=llvm3_5
                             then [("createScalarizerPass","FunctionPass",[])
                                  ,("createAddDiscriminatorsPass","FunctionPass",[])
                                  ,("createSeparateConstOffsetFromGEPPass","FunctionPass",[])
                                  ,("createLoadCombinePass","BasicBlockPass",[])]
                             else [])++
        (if version<=llvm3_2
         then [("createSimplifyLibCallsPass","FunctionPass",[])]
         else [])
       ]++
       [Spec { specHeader = "llvm/Transforms/IPO.h"
             , specNS = llvmNS
             , specName = f
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType p
                                        , gfunArgs = fmap (\x -> (False,x)) a
                                        , gfunHSName = f
                                        }
             } | (p,f,a) <- [("ModulePass","createStripSymbolsPass",[normalT bool])
                           ,("ModulePass","createStripNonDebugSymbolsPass",[])
                           ,("ModulePass","createStripDebugDeclarePass",[])
                           ,("ModulePass","createStripDeadDebugInfoPass",[])
                           ,("ModulePass","createConstantMergePass",[])
                           ,("ModulePass","createGlobalOptimizerPass",[])
                           ,("ModulePass","createGlobalDCEPass",[])
                           ,("Pass","createFunctionInliningPass",[normalT int])
                           ,("Pass","createAlwaysInlinerPass",if version>=llvm3_1
                                                              then [normalT bool]
                                                              else [])
                           ,("Pass","createPruneEHPass",[])
                           ,("ModulePass","createInternalizePass",[if version>=llvm3_0
                                                                   then normalT $ NamedType llvmNS "ArrayRef" [constT $ ptr $ char] False
                                                                   else constT $ NamedType [ClassName "std" []] "vector" [constT $ ptr char] False])
                           ,("ModulePass","createDeadArgEliminationPass",[])
                           ,("ModulePass","createDeadArgHackingPass",[])
                           ,("Pass","createArgumentPromotionPass",[normalT unsigned])
                           ,("ModulePass","createIPConstantPropagationPass",[])
                           ,("ModulePass","createIPSCCPPass",[])
                           ,("Pass","createLoopExtractorPass",[])
                           ,("Pass","createSingleLoopExtractorPass",[])
                           ,("ModulePass","createBlockExtractorPass",[])
                           ,("ModulePass","createStripDeadPrototypesPass",[])
                           ,("Pass","createFunctionAttrsPass",[])
                           ,("ModulePass","createMergeFunctionsPass",[])
                           ,("ModulePass","createPartialInliningPass",[])]++
        (if version>=llvm3_3
         then [("ModulePass","createMetaRenamerPass",[])
              ,("ModulePass","createBarrierNoopPass",[])]
         else [])
       ]++
       [Spec { specHeader = if version<llvm3_5
                            then "llvm/Analysis/Verifier.h"
                            else "llvm/IR/Verifier.h"
             , specNS = llvmNS
             , specName = "createVerifierPass"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "FunctionPass"
                                        , gfunArgs = []
                                        , gfunHSName = "createVerifierPass"
                                        }
             }]++
       [Spec { specHeader = "llvm/Analysis/Passes.h"
             , specNS = llvmNS
             , specName = f
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType p
                                        , gfunArgs = fmap (\x -> (False,x)) a
                                        , gfunHSName = f
                                        }
             } | (p,f,a) <- [("Pass","createGlobalsModRefPass",[])
                            ,("Pass","createAliasDebugger",[])
                            ,("ModulePass","createAliasAnalysisCounterPass",[])
                            ,("FunctionPass","createAAEvalPass",[])
                            ,("ImmutablePass","createNoAAPass",[])
                            ,("ImmutablePass","createBasicAliasAnalysisPass",[])
                            {-,("FunctionPass","createLibCallAliasAnalysisPass",
                              [normalT $ ptr $ llvmType "LibCallInfo"])-}
                            ,("FunctionPass","createScalarEvolutionAliasAnalysisPass",[])
                            ,("ImmutablePass","createTypeBasedAliasAnalysisPass",[])]++
                            (if version>=llvm3_0
                             then [("ImmutablePass","createObjCARCAliasAnalysisPass",[])]
                             else [])++
                            [("FunctionPass","createLazyValueInfoPass",[])]++
                            (if version>=llvm3_2
                             then [("FunctionPass","createDependenceAnalysisPass",[])
                                  ,("FunctionPass","createCostModelAnalysisPass",[])]
                             else [])++
                            (if version>=llvm3_4
                             then [("FunctionPass","createDelinearizationPass",[])]
                             else [])++
                            [("FunctionPass","createInstCountPass",[])
                            ,("FunctionPass","createRegionInfoPass",[])
                            ,("ModulePass","createModuleDebugInfoPrinterPass",[])]++
                            (if version>=llvm2_9
                             then [("FunctionPass","createMemDepPrinter",[])]
                             else [])++
                            (if version>=llvm3_5
                             then [("ImmutablePass","createJumpInstrTableInfoPass",[])]
                             else [])
       ]++
       [Spec { specHeader = "llvm/Support/raw_ostream.h"
             , specNS = llvmNS
             , specName = "raw_ostream"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Destructor True,"deleteOStream_")
                          ]
             }
       ,Spec { specHeader = "llvm/Support/raw_ostream.h"
             , specNS = llvmNS
             , specName = "raw_fd_ostream"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [(False,normalT int),(False,normalT bool),(False,normalT bool)],"newFDOStream_")
                          ,(Destructor False,"deleteFDOStream")]
             }
       ,Spec { specHeader = "llvm/Support/raw_ostream.h"
             , specNS = llvmNS
             , specName = "raw_string_ostream"
             , specTemplateArgs = []
             , specType = classSpec
                           [(Constructor [(False,normalT $ ref $ NamedType [ClassName "std" []] "string" [] False)],"newStringOStream")]
             }
       ,Spec { specHeader = "llvm/Analysis/AliasAnalysis.h"
             , specNS = llvmNS
             , specName = "AliasAnalysis"
             , specTemplateArgs = []
             , specType = classSpec $
                          [(Constructor [],"newAliasAnalysis")
                          ,(Destructor True,"deleteAliasAnalysis_")]++
                          (if version>=llvm3_3
                           then [(memberFun { ftReturnType = constT $ ptr $ llvmType "TargetLibraryInfo"
                                            , ftName = "getTargetLibraryInfo"
                                            , ftOverloaded = True
                                            },"aliasAnalysisGetTargetLibraryInfo_")]
                           else [])++
                          [(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getTypeStoreSize"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                      , ftOverloaded = True
                                      },"aliasAnalysisGetTypeStoreSize_")]++
                          (if version>=llvm2_9
                           then [(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" [] False
                                            , ftName = "getLocation"
                                            , ftArgs = [(False,constT $ ptr $ llvmType (inst++"Inst"))]
                                            , ftOverloaded = True
                                            },"aliasAnalysisGetLocation"++inst++"_")
                                 | inst <- ["Load","Store","VAArg"]++(if version>=llvm3_0
                                                                     then ["AtomicCmpXchg","AtomicRMW"]
                                                                     else [])]
                           else [])++
                          [(memberFun { ftReturnType = normalT $ EnumType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "AliasResult"
                                      , ftName = "alias"
                                      , ftArgs = if version>=llvm2_9
                                                 then [(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" [] False)
                                                      ,(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" [] False)]
                                                 else [(False,constT $ ptr $ llvmType "Value")
                                                      ,(False,normalT unsigned)
                                                      ,(False,constT $ ptr $ llvmType "Value")
                                                      ,(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },"aliasAnalysisAlias_")
                          ]
             }
       ]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/Analysis/AliasAnalysis.h"
                , specNS = [ClassName "llvm" [],ClassName "AliasAnalysis" []]
                , specName = "Location"
                , specTemplateArgs = []
                , specType = classSpecCustom
                             ("data Location = Location { locationPtr :: Ptr Value, locationSize :: Word64, locationAATags :: "++
                              (if version>=llvm3_6
                               then "AAMDNodes"
                               else "Ptr MDNode")++" }") $
                             [(Constructor [(True,constT $ ptr $ llvmType "Value")
                                           ,(False,normalT uint64_t)
                                           ,(False,constT $ ptr $ llvmType "MDNode")
                                           ],"newLocation_")
                             ,(SizeOf,"locationSizeOf")
                             ,(AlignOf,"locationAlignOf")
                             ,(Getter "Ptr" (constT $ ptr $ llvmType "Value") False,
                               "locationGetPtr")
                             ,(Setter "Ptr" (constT $ ptr $ llvmType "Value"),
                               "locationSetPtr")
                             ,(Getter "Size" (normalT uint64_t) False,
                               "locationGetSize")
                             ,(Setter "Size" (normalT uint64_t),
                               "locationSetSize")]++
                             (if version>=llvm3_6
                              then [(Getter "AATags" (normalT $ llvmType "AAMDNodes") False,
                                     "locationGetAATags")
                                   ,(Setter "AATags" (normalT $ llvmType "AAMDNodes"),
                                     "locationSetAATags")]
                              else [(Getter "TBAATag" (normalT $ ptr $ llvmType "MDNode") False,
                                     "locationGetTBAATag")
                                   ,(Setter "TBAATag" (normalT $ ptr $ llvmType "MDNode"),
                                     "locationSetTBAATag")])
                              
                }]
     else [])++
       [Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = "getMallocAllocatedType"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "Type"
                                        , gfunArgs = [(False,constT $ ptr $ llvmType "CallInst")]++
                                                     (if version>=llvm3_2
                                                      then [(False,constT $ ptr $ llvmType "TargetLibraryInfo")]
                                                      else [])
                                        , gfunHSName = "getMallocAllocatedType"
                                        }
             }
       ,Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = "getMallocArraySize"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "Value"
                                        , gfunArgs = [(False,normalT $ ptr $ llvmType "CallInst")]++
                                                     (if version >= llvm3_2
                                                      then [(False,constT $ ptr $ llvmType "DataLayout")
                                                           ,(False,constT $ ptr $ llvmType "TargetLibraryInfo")]
                                                      else [(False,constT $ ptr $ llvmType "TargetData")])++
                                                     [(False,normalT bool)]
                                        , gfunHSName = "getMallocArraySize"
                                        }
             }
       ,Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = if version>=llvm3_2
                          then "isMallocLikeFn"
                          else "isMalloc"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT bool
                                        , gfunArgs = [(True,constT $ ptr $ llvmType "Value")]++
                                                     (if version>=llvm3_2
                                                      then [(False,constT $ ptr $ llvmType "TargetLibraryInfo")
                                                           ,(False,normalT bool)]
                                                      else [])
                                        , gfunHSName = "isMallocLikeFn_"
                                        }
             }
       ,Spec { specHeader = "llvm/ADT/Twine.h"
             , specNS = llvmNS
             , specName = "Twine"
             , specTemplateArgs = []
             , specType = classSpec
                          [(Constructor [],"newTwineEmpty")
                          ,(Constructor [(False,constT $ ptr char)],"newTwineString_")
                          ]
             }
       ,Spec { specHeader = "llvm/Analysis/LoopInfo.h"
             , specNS = llvmNS
             , specName = "Loop"
             , specTemplateArgs = []
             , specType = classSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isLoopInvariant"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")]
                                      },"loopIsLoopInvariant_")]
             }
       ]++
    concat
    [[Spec { specHeader = "llvm/Analysis/LoopInfo.h"
           , specNS = llvmNS
           , specName = "LoopBase"
           , specTemplateArgs = [blk,loop]
           , specType = classSpec
                        [(memberFun { ftReturnType = normalT unsigned
                                    , ftName = "getLoopDepth"
                                    , ftOverloaded = True
                                    },"loopGetDepth_")
                        ,(memberFun { ftReturnType = toPtr blk
                                    , ftName = "getHeader"
                                    , ftOverloaded = True
                                    },"loopGetHeader_")
                        ,(memberFun { ftReturnType = toPtr loop
                                    , ftName = "getParentLoop"
                                    , ftOverloaded = True
                                    },"loopGetParent_")
                        ,(memberFun { ftReturnType = normalT bool
                                    , ftName = "contains"
                                    , ftArgs = [(False,toConstPtr loop)]
                                    , ftOverloaded = True
                                    },"loopContainsLoop_")
                        ,(memberFun { ftReturnType = normalT bool
                                    , ftName = "contains"
                                    , ftArgs = [(False,toConstPtr blk)]
                                    , ftOverloaded = True
                                    },"loopContainsBlock_")
                        ,(memberFun { ftReturnType = constT $ NamedType [ClassName "std" []] "vector" [toPtr loop] False
                                    , ftName = "getSubLoops"
                                    , ftOverloaded = True
                                    },"loopGetSubLoops_")
                        ,(memberFun { ftReturnType = constT $ NamedType [ClassName "std" []] "vector" [toPtr blk] False
                                    , ftName = "getBlocks"
                                    , ftOverloaded = True
                                    },"loopGetBlocks_")
                        ,(memberFun { ftName = "getExitEdges"
                                    , ftArgs = [(False,normalT $ ref $ NamedType llvmNS "SmallVector"
                                                          [normalT $ NamedType [ClassName "std" []] "pair" [constT $ ptr $ llvmType "BasicBlock"
                                                                                                           ,constT $ ptr $ llvmType "BasicBlock"] False
                                                          ,TypeInt 16] False)]
                                    , ftOverloaded = True
                                    },"loopGetExitEdges_")
                        ,(memberFun { ftReturnType = normalT unsigned
                                    , ftName = "getNumBackEdges"
                                    , ftOverloaded = True
                                    },"loopGetNumBackEdges_")]
             }
     ,Spec { specHeader = "llvm/Analysis/LoopInfo.h"
           , specNS = llvmNS
           , specName = "LoopInfoBase"
           , specTemplateArgs = [blk,loop]
           , specType = classSpec
                        [(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" []
                                                                         ,ClassName "vector" [toPtr loop]
                                                                         ] "const_iterator" [] False
                                    , ftName = "begin"
                                    },"loopInfoBaseBegin_")
                        ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" []
                                                                         ,ClassName "vector" [toPtr loop]
                                                                         ] "const_iterator" [] False
                                    , ftName = "end"
                                    },"loopInfoBaseEnd_")
                        ,(memberFun { ftReturnType = toPtr loop
                                    , ftName = "getLoopFor"
                                    , ftArgs = [(False,toConstPtr blk)]
                                    },"loopInfoBaseGetLoopFor_")]
           }]
     | (blk,loop) <- [(normalT $ llvmType "BasicBlock",normalT $ llvmType "Loop")]
    ]++
    [Spec { specHeader = "llvm/PassAnalysisSupport.h"
          , specNS = llvmNS
          , specName = "AnalysisUsage"
          , specTemplateArgs = []
          , specType = classSpec
                       [(Constructor [],"newAnalysisUsage")
                       ,(memberFun { ftName = "addRequiredID"
                                   , ftArgs = [(False,normalT $ ref char)]
                                   , ftIgnoreReturn = True
                                   },"analysisUsageAddRequired_")
                       ,(memberFun { ftName = "addRequiredTransitiveID"
                                   , ftArgs = [(False,normalT $ ref char)]
                                   , ftIgnoreReturn = True
                                   },"analysisUsageAddRequiredTransitive_")
                       ,(memberFun { ftName = "addPreservedID"
                                   , ftArgs = [(False,normalT $ ref char)]
                                   , ftIgnoreReturn = True
                                   },"analysisUsageAddPreserved_")
                       ,(memberFun { ftName = "setPreservesAll"
                                   },"analysisUsagePreservesAll")
                       ,(memberFun { ftName = "setPreservesCFG"
                                   },"analysisUsagePreservesCFG")]
          }
    ,Spec { specHeader = "llvm/PassAnalysisSupport.h"
          , specNS = llvmNS
          , specName = "AnalysisResolver"
          , specTemplateArgs = []
          , specType = classSpec
                       [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Pass"
                                   , ftName = "findImplPass"
                                   , ftArgs = [(False,constT $ ptr void)]
                                   },"analysisResolverFindImplPass_")
                       ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Pass"
                                   , ftName = "findImplPass"
                                   , ftArgs = [(True,normalT $ ptr $ llvmType "Pass")
                                              ,(False,constT $ ptr void)
                                              ,(False,normalT $ ref $ llvmType "Function")]
                                   },"analysisResolverFindImplPassFun_")]
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/GenericValue.h"
          , specNS = llvmNS
          , specName = "GenericValue"
          , specTemplateArgs = []
          , specType = classSpecCustom ("data GenericValue = GDouble CDouble | GFloat CFloat | GPointer (Ptr ()) | GIntPair CUInt CUInt | GInt APInt "++
                                        (if version>=llvm3_3
                                         then "| GAggregate [GenericValue]"
                                         else "")++"deriving (Show,Typeable,Eq,Ord)") $
                       [(Constructor [],"newGenericValue")
                       ,(Getter { ftGetVar = "DoubleVal"
                                , ftGetType = normalT double
                                , ftGetStatic = False
                                },"genericValueGetDouble")
                       ,(Setter { ftSetVar = "DoubleVal"
                                , ftSetType = normalT double
                                },"genericValueSetDouble")
                       ,(Getter { ftGetVar = "FloatVal"
                                , ftGetType = normalT float
                                , ftGetStatic = False
                                },"genericValueGetFloat")
                       ,(Setter { ftSetVar = "FloatVal"
                                , ftSetType = normalT float
                                },"genericValueSetFloat")
                       ,(Getter { ftGetVar = "PointerVal"
                                , ftGetType = normalT $ ptr void
                                , ftGetStatic = False
                                },"genericValueGetPointer")
                       ,(Setter { ftSetVar = "PointerVal"
                                , ftSetType = normalT $ ptr void
                                },"genericValueSetPointer")
                       ,(Getter { ftGetVar = "IntVal"
                                , ftGetType = normalT $ llvmType "APInt"
                                , ftGetStatic = False
                                },"genericValueGetInt")
                       ,(Setter { ftSetVar = "IntVal"
                                , ftSetType = normalT $ llvmType "APInt"
                                },"genericValueSetInt")]++
                       (if version>=llvm3_3
                        then [(Getter { ftGetVar = "AggregateVal"
                                      , ftGetType = normalT $ NamedType [ClassName "std" []] "vector"
                                                    [normalT $ llvmType "GenericValue"]
                                                    False
                                      , ftGetStatic = False
                                      },"genericValueGetAggregate")
                             ,(Setter { ftSetVar = "AggregateVal"
                                      , ftSetType = normalT $ NamedType [ClassName "std" []] "vector"
                                                    [normalT $ llvmType "GenericValue"]
                                                    False
                                      },"genericValueSetAggregate")]
                        else [])
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/ExecutionEngine.h"
          , specNS = llvmNS
          , specName = "ExecutionEngine"
          , specTemplateArgs = []
          , specType = classSpec $
                       [(Destructor True,"deleteExecutionEngine_")
                       ,(memberFun { ftName = "addModule"
                                   , ftArgs = [(False,normalT $ ptr $ llvmType "Module")]
                                   , ftOverloaded = True
                                   },"executionEngineAddModule_")
                       ,if version >= llvm3_2
                        then (memberFun { ftReturnType = normalT $ ptr $ llvmType "DataLayout"
                                        , ftName = "getDataLayout"
                                        , ftOverloaded = True
                                        },"executionEngineGetDataLayout_")
                        else (memberFun { ftReturnType = normalT $ ptr $ llvmType "TargetData"
                                        , ftName = "getTargetData"
                                        , ftOverloaded = True
                                        },"executionEngineGetTargetData_")
                       ,(memberFun { ftReturnType = normalT bool
                                   , ftName = "removeModule"
                                   , ftArgs = [(False,normalT $ ptr $ llvmType "Module")]
                                   , ftOverloaded = True
                                   },"executionEngineRemoveModule_")
                       ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                   , ftName = "FindFunctionNamed"
                                   , ftArgs = [(False,constT $ ptr char)]
                                   , ftOverloaded = True
                                   },"executionEngineFindFunctionNamed_")
                       ,(memberFun { ftReturnType = normalT $ llvmType "GenericValue"
                                   , ftName = "runFunction"
                                   , ftArgs = [(False,normalT $ ptr $ llvmType "Function")
                                              ,(False,constT $ ref $ NamedType [ClassName "std" []] "vector"
                                                         [normalT $ llvmType "GenericValue"] False)]
                                   , ftOverloaded = True
                                   },"executionEngineRunFunction_")]++
            (if version >= llvm3_1
             then [(memberFun { ftReturnType = normalT $ ptr void
                              , ftName = "getPointerToNamedFunction"
                              , ftArgs = [(False,constT $ ptr char)
                                         ,(False,normalT bool)]
                              , ftOverloaded = True
                              },"executionEngineGetPointerToNamedFunction_")
                  ,(memberFun { ftName = "mapSectionAddress"
                        , ftArgs = [(False,if version >= llvm3_2
                                           then constT $ ptr void
                                           else normalT $ ptr void)
                                   ,(False,normalT uint64_t)]
                                   , ftOverloaded = True
                        },"executionEngineMapSectionAddress_")]
             else [])++
            [(memberFun { ftName = "runStaticConstructorsDestructors"
                        , ftArgs = [(False,normalT bool)]
                        , ftOverloaded = True
                        },"executionEngineRunStaticConstructorsDestructors_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToFunction"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Function")]
                        , ftOverloaded = True
                        },"executionEngineGetPointerToFunction_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToFunctionOrStub"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Function")]
                        , ftOverloaded = True
                        },"executionEngineGetPointerToFunctionOrStub_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToGlobal"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")]
                        , ftOverloaded = True
                        },"executionEngineGetPointerToGlobal_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToGlobalIfAvailable"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")]
                        , ftOverloaded = True
                        },"executionEngineGetPointerToGlobalIfAvailable_")
            ,(memberFun { ftName = "addGlobalMapping"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")
                                   ,(False,normalT $ ptr void)]
                        , ftOverloaded = True
                        },"executionEngineAddGlobalMapping_")
            ,(memberFun { ftName = "clearAllGlobalMappings"
                        , ftOverloaded = True
                        },"executionEngineClearAllGlobalMappings_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "updateGlobalMapping"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")
                                   ,(False,normalT $ ptr void)]
                        , ftOverloaded = True
                        },"executionEngineUpdateGlobalMapping_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToBasicBlock"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "BasicBlock")]
                        , ftOverloaded = True
                        },"executionEngineGetPointerToBasicBlock_")]++                        
            (if version<llvm3_6
             then [(memberFun { ftName = "runJITOnFunction"
                              , ftArgs = [(False,normalT $ ptr $ llvmType "Function")
                                         ,(False,normalT $ ptr $ llvmType "MachineCodeInfo")]
                              , ftOverloaded = True
                              },"executionEngineRunJITOnFunction_")]
             else [])++
            [(memberFun { ftReturnType = constT $ ptr $ llvmType "GlobalValue"
                        , ftName = "getGlobalValueAtAddress"
                        , ftArgs = [(False,normalT $ ptr void)]
                        , ftOverloaded = True
                        },"executionEngineGetGlobalValueAtAddress_")
            ,(memberFun { ftName = "StoreValueToMemory"
                        , ftArgs = [(False,constT $ ref $ llvmType "GenericValue")
                                   ,(False,normalT $ ptr $ llvmType "GenericValue")
                                   ,(True,normalT $ ptr $ llvmType "Type")]
                        , ftOverloaded = True
                        },"executionEngineStoreValueToMemory_")
            ,(memberFun { ftName = "InitializeMemory"
                        , ftArgs = [(True,constT $ ptr $ llvmType "Constant")
                                   ,(False,normalT $ ptr void)]
                        , ftOverloaded = True
                        },"executionEngineInitializeMemory_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "recompileAndRelinkFunction"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Function")]
                        , ftOverloaded = True
                        },"executionEngineRecompileAndRelinkFunction_")
            ,(memberFun { ftName = "freeMachineCodeForFunction"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Function")]
                        , ftOverloaded = True
                        },"executionEngineFreeMachineCodeForFunction_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getOrEmitGlobalVariable"
                        , ftArgs = [(False,constT $ ptr $ llvmType "GlobalVariable")]
                        , ftOverloaded = True
                        },"executionEngineGetOrEmitGlobalVariable_")
            ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ExecutionEngine"
                        , ftName = "create"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Module")
                                   ,(False,normalT bool)
                                   ,(False,normalT $ ptr $ NamedType [ClassName "std" []] "string" [] False)
                                   ,(False,normalT $ EnumType [ClassName "llvm" []
                                                              ,ClassName "CodeGenOpt" []
                                                              ] "Level")
                                   ,(False,normalT bool)]
                        , ftStatic = True
                        },"newExecutionEngine_")
            ]
          }
    ,Spec { specHeader = if version>=llvm3_0
                         then "llvm/Support/CodeGen.h"
                         else "llvm/Target/TargetMachine.h"
          , specNS = [ClassName "llvm" []
                     ,ClassName "CodeGenOpt" []]
          , specName = "Level"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "CodeGenOptLevel"
                                 [Right $ EnumLeaf "None" "CodeGenOptNone"
                                 ,Right $ EnumLeaf "Less" "CodeGenOptLess"
                                 ,Right $ EnumLeaf "Default" "CodeGenOptDefault"
                                 ,Right $ EnumLeaf "Aggressive" "CodeGenOptAggressive"])
          }
    ,Spec { specHeader = if version>=llvm3_0
                         then "llvm/Support/CodeGen.h"
                         else "llvm/Target/TargetMachine.h"
          , specNS = [ClassName "llvm" []
                     ,ClassName "CodeModel" []]
          , specName = "Model"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "CodeModel" $
                                 [Right $ EnumLeaf "Default" "CodeModelDefault"
                                 ,Right $ EnumLeaf "Small" "CodeModelSmall"
                                 ,Right $ EnumLeaf "Kernel" "CodeModelKernel"
                                 ,Right $ EnumLeaf "Medium" "CodeModelMedium"
                                 ,Right $ EnumLeaf "Large" "CodeModelLarge"]++
                                 (if version>=llvm3_0
                                  then [Right $ EnumLeaf "JITDefault" "CodeModelJITDefault"]
                                  else []))
          }
    ,Spec { specHeader = if version>=llvm3_0
                         then "llvm/Support/CodeGen.h"
                         else "llvm/Target/TargetMachine.h"
          , specNS = [ClassName "llvm" []
                     ,ClassName "Reloc" []]
          , specName = "Model"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "RelocModel"
                                 [Right $ EnumLeaf "Default" "RelocModelDefault"
                                 ,Right $ EnumLeaf "Static" "RelocModelStatic"
                                 ,Right $ EnumLeaf "PIC_" "RelocModelPIC"
                                 ,Right $ EnumLeaf "DynamicNoPIC" "RelocModelDynamicNoPIC"])
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/ExecutionEngine.h"
          , specNS = llvmNS
          , specName = "EngineBuilder"
          , specTemplateArgs = []
          , specType = classSpec $
                       [(Constructor [(False,normalT $ ptr $ llvmType "Module")],"newEngineBuilder")
                       ,(Destructor False,"deleteEngineBuilder")
                       ,(memberFun { ftIgnoreReturn = True
                                   , ftName = "setEngineKind"
                                   , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" []
                                                                         ,ClassName "EngineKind" []
                                                                         ] "Kind")]
                                   },"engineBuilderSetKind_")
                       ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ExecutionEngine"
                                   , ftName = "create"
                                   },"engineBuilderCreate")
                       ,(memberFun { ftIgnoreReturn = True
                                   , ftName = "setOptLevel"
                                   , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" []
                                                                         ,ClassName "CodeGenOpt" []
                                                                         ] "Level")]
                                   },"engineBuilderSetOptLevel_")
                       ,(memberFun { ftIgnoreReturn = True
                                   , ftName = "setCodeModel"
                                   , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" []
                                                                         ,ClassName "CodeModel" []
                                                                         ] "Model")]
                                   },"engineBuilderSetCodeModel_")
                       ,(memberFun { ftIgnoreReturn = True
                                   , ftName = "setErrorStr"
                                   , ftArgs = [(False,normalT $ ptr $ NamedType [ClassName "std" []] "string" [] False)]
                                   },"engineBuilderSetErrorStr")]++
            (if version>=llvm3_0
             then [(memberFun { ftIgnoreReturn = True
                              , ftName = "setRelocationModel"
                              , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" []
                                                                    ,ClassName "Reloc" []
                                                                    ] "Model")]
                              },"engineBuilderSetRelocationModel_")]
             else [])
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/ExecutionEngine.h"
          , specNS = [ClassName "llvm" [],ClassName "EngineKind" []]
          , specName = "Kind"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "EngineKind" [Right $ EnumLeaf "JIT" "JIT"
                                                       ,Right $ EnumLeaf "Interpreter" "Interpreter"
                                                       ,Right $ EnumLeaf "Either" "EitherEngine"])
          }]++
    (if version<llvm3_6
     then [Spec { specHeader = "llvm/CodeGen/MachineCodeInfo.h"
                , specNS = llvmNS
                , specName = "MachineCodeInfo"
                , specTemplateArgs = []
                , specType = classSpec
                             [(Constructor [],"newMachineCodeInfo")
                             ,(memberFun { ftName = "setSize"
                                         , ftArgs = [(False,normalT size_t)]
                                         },"machineCodeInfoSetSize")
                             ,(memberFun { ftName = "setAddress"
                                         , ftArgs = [(False,normalT $ ptr void)]
                                         },"machineCodeInfoSetAddress")
                             ,(memberFun { ftReturnType = normalT size_t
                                         , ftName = "size"
                                         },"machineCodeInfoGetSize")
                             ,(memberFun { ftReturnType = normalT $ ptr void
                                         , ftName = "address"
                                         },"machineCodeInfoGetAddress")]
                }]
     else [])++
    [Spec { specHeader = "llvm/Pass.h"
          , specNS = llvmNS
          , specName = "PassKind"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "PassKind"
                                 [Right $ EnumLeaf ("PT_"++name) ("PassKind"++name)
                                 | name <- ["BasicBlock"]++
                                           (if version>=llvm2_9
                                            then ["Region"]
                                            else [])++
                                           ["Loop"
                                           ,"Function"
                                           ,"CallGraphSCC"
                                           ,"Module"
                                           ,"PassManager"]])
          }
    ,Spec { specHeader = irInclude version "InstrTypes.h"
          , specNS = [ClassName "llvm" [],ClassName "CmpInst" []]
          , specName = "Predicate"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "Predicate"
                                 [Left ("ICmpOp",EnumNode "ICmpOp"
                                        [Right $ EnumLeaf ("ICMP_"++name) ("I_"++name)
                                        | name <- ["EQ","NE","UGT","UGE"
                                                  ,"ULT","ULE","SGT","SGE"
                                                  ,"SLT","SLE"]])
                                 ,Left ("FCmpOp",EnumNode "FCmpOp"
                                        [Right $ EnumLeaf ("FCMP_"++name) ("F_"++name)
                                        | name <- ["OEQ","OGT","OGE","OLT"
                                                  ,"OLE","ONE","ORD","UNO"
                                                  ,"UEQ","UGT","UGE","ULT"
                                                  ,"ULE","UNE"]])])
          }
    ,Spec { specHeader = irInclude version "CallingConv.h"
          , specNS = [ClassName "llvm" [],ClassName "CallingConv" []]
          , specName = "ID"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "CallingConv"
                                 [Right $ EnumLeaf name name
                                 | name <- ["C","Fast","Cold","GHC"
                                           ,"FirstTargetCC"
                                           ,"X86_StdCall","X86_FastCall"
                                           ,"ARM_APCS","ARM_AAPCS"
                                           ,"ARM_AAPCS_VFP"
                                           ,"MSP430_INTR"
                                           ,"X86_ThisCall"]++
                                           (if version>=llvm2_9
                                            then ["PTX_Kernel","PTX_Device"]++
                                                 (if version<=llvm3_3
                                                  then ["MBLAZE_INTR","MBLAZE_SVOL"]
                                                  else [])
                                            else [])++
                                           (if version>llvm3_1
                                            then ["SPIR_FUNC"
                                                 ,"SPIR_KERNEL"
                                                 ,"Intel_OCL_BI"]
                                            else [])])
          }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "SynchronizationScope"
                , specTemplateArgs = []
                , specType = EnumSpec (EnumNode "SynchronizationScope"
                                       [Right $ EnumLeaf "SingleThread" "SingleThread"
                                       ,Right $ EnumLeaf "CrossThread" "CrossThread"])
                }
          ,Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "AtomicOrdering"
                , specTemplateArgs = []
                , specType = EnumSpec (EnumNode "AtomicOrdering"
                                       [Right $ EnumLeaf name name
                                       | name <- ["NotAtomic"
                                                 ,"Unordered"
                                                 ,"Monotonic"
                                                 ,"Acquire"
                                                 ,"Release"
                                                 ,"AcquireRelease"
                                                 ,"SequentiallyConsistent"]])
                }
          ,Spec { specHeader = irInclude version "Instructions.h"
                , specNS = [ClassName "llvm" [],ClassName "AtomicRMWInst" []]
                , specName = "BinOp"
                , specTemplateArgs = []
                , specType = EnumSpec (EnumNode "RMWBinOp"
                                       [Right $ EnumLeaf name ("RMW"++name)
                                       | name <- ["Xchg"
                                                 ,"Add"
                                                 ,"Sub"
                                                 ,"And"
                                                 ,"Nand"
                                                 ,"Or"
                                                 ,"Xor"
                                                 ,"Max"
                                                 ,"Min"
                                                 ,"UMax"
                                                 ,"UMin"]])
                }]
     else [])++
    [Spec { specHeader = "llvm/Analysis/AliasAnalysis.h"
          , specNS = [ClassName "llvm" [],ClassName "AliasAnalysis" []]
          , specName = "AliasResult"
          , specTemplateArgs = []
          , specType = EnumSpec (EnumNode "AliasResult" $
                                 [Right $ EnumLeaf "NoAlias" "NoAlias"
                                 ,Right $ EnumLeaf "MayAlias" "MayAlias"]++
                                 (if version>=llvm2_9
                                  then [Right $ EnumLeaf "PartialAlias" "PartialAlias"]
                                  else [])++
                                 [Right $ EnumLeaf "MustAlias" "MustAlias"])
          }
    ,Spec { specHeader = if version>=llvm3_0
                         then "llvm/Support/TargetRegistry.h"
                         else "llvm/Target/TargetRegistry.h"
          , specNS = llvmNS
          , specName = "Target"
          , specTemplateArgs = []
          , specType = classSpec $
                       [(memberFun { ftReturnType = constT $ ptr $ llvmType "Target"
                                   , ftName = "getNext"
                                   },"targetNext")
                       ,(memberFun { ftReturnType = constT $ ptr char
                                   , ftName = "getName"
                                   },"targetName")
                       ,(memberFun { ftReturnType = constT $ ptr char
                                   , ftName = "getShortDescription"
                                   },"targetShortDescription")]++
                       [(memberFun { ftReturnType = normalT bool
                                   , ftName = "has"++name
                                   },"targetHas"++name)
                        | name <- ["JIT","TargetMachine"]++
                                  (if version<=llvm3_3
                                   then ["AsmPrinter"]
                                   else [])++
                                 (if version>=llvm2_9 && version<=llvm3_3
                                  then ["AsmStreamer"]
                                  else [])++
                                 (if version>=llvm3_0
                                  then ["MCAsmBackend"]++
                                       (if version<=llvm3_3
                                        then ["MCAsmParser","MCDisassembler","MCInstPrinter"
                                             ,"MCCodeEmitter","MCObjectStreamer"]
                                        else [])
                                  else []) ]
          }
    ,Spec { specHeader = "llvm/Target/TargetMachine.h"
          , specNS = llvmNS
          , specName = "TargetMachine"
          , specTemplateArgs = []
          , specType = classSpec
                       [(Destructor True,"deleteTargetMachine_")
                       ,(memberFun { ftReturnType = constT $ ref $ llvmType "Target"
                                   , ftName = "getTarget"
                                   , ftOverloaded = True
                                   },"targetMachineTarget_")]
          }
    ,Spec { specHeader = if version>=llvm3_0
                         then "llvm/Support/TargetRegistry.h"
                         else "llvm/Target/TargetRegistry.h"
          , specNS = llvmNS
          , specName = "TargetRegistry"
          , specTemplateArgs = []
          , specType = classSpec $
                       (if version>=llvm3_0
                        then [(memberFun { ftName = "printRegisteredTargetsForVersion"
                                         , ftStatic = True
                                         },"targetRegistryPrintTargets")]
                        else [])++
                       [(memberFun { ftReturnType = constT $ ptr $ llvmType "Target"
                                   , ftName = "lookupTarget"
                                   , ftStatic = True
                                   , ftArgs = [(False,constT $ ref $ NamedType [ClassName "std" []] "string" [] False)
                                              ,(False,normalT $ ref $ NamedType [ClassName "std" []] "string" [] False)]
                                   },"targetRegistryLookup_")]++
                       (if version>=llvm3_2
                        then [(memberFun { ftReturnType = constT $ ptr $ llvmType "Target"
                                         , ftName = "lookupTarget"
                                         , ftStatic = True
                                         , ftArgs = [(False,constT $ ref $ NamedType [ClassName "std" []] "string" [] False)
                                                    ,(False,normalT $ ref $ llvmType "Triple")
                                                    ,(False,normalT $ ref $ NamedType [ClassName "std" []] "string" [] False)]
                                         },"targetRegistryLookupTriple_")]
                        else [])
          }
    ,Spec { specHeader = "llvm/ADT/Triple.h"
          , specNS = llvmNS
          , specName = "Triple"
          , specTemplateArgs = []
          , specType = classSpec $
                       [(Constructor [],"newTripleEmpty")
                       ,(Constructor [(False,if version>=llvm3_0
                                             then constT $ ref $ llvmType "Twine"
                                             else normalT $ llvmType "StringRef")],
                         "newTripleFromString")
                       ,(memberFun { ftReturnType = normalT $ EnumType [ClassName "llvm" [],
                                                                        ClassName "Triple" []] "ArchType"
                                   , ftName = "getArch"
                                   },"tripleGetArch_")
                       ,(memberFun { ftReturnType = normalT $ EnumType [ClassName "llvm" [],
                                                                        ClassName "Triple" []] "VendorType"
                                   , ftName = "getVendor"
                                   },"tripleGetVendor_")
                       ,(memberFun { ftReturnType = normalT $ EnumType [ClassName "llvm" [],
                                                                        ClassName "Triple" []] "OSType"
                                   , ftName = "getOS"
                                   },"tripleGetOS_")
                       ,(memberFun { ftReturnType = normalT bool
                                   , ftName = "hasEnvironment"
                                   },"tripleHasEnvironment")]++
                       (if version>=llvm2_9
                        then [(memberFun { ftReturnType = normalT $
                                                          EnumType [ClassName "llvm" [],
                                                                    ClassName "Triple" []] "EnvironmentType"
                                         , ftName = "getEnvironment"
                                         },"tripleGetEnvironment_")]
                        else [])++
                       (if version>=llvm3_0
                        then [(memberFun { ftName = "getOSVersion"
                                         , ftArgs = [(False,normalT $ ref unsigned)
                                                    ,(False,normalT $ ref unsigned)
                                                    ,(False,normalT $ ref unsigned)]
                                         },"tripleGetOSVersion_")]
                        else [])++
                       (if version>=llvm3_1
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "getMacOSXVersion"
                                         , ftArgs = [(False,normalT $ ref unsigned)
                                                    ,(False,normalT $ ref unsigned)
                                                    ,(False,normalT $ ref unsigned)]
                                         },"tripleGetMacOSXVersion_")]
                        else [])++
                       (if version>=llvm3_2
                        then [(memberFun { ftName = "getiOSVersion"
                                         , ftArgs = [(False,normalT $ ref unsigned)
                                                    ,(False,normalT $ ref unsigned)
                                                    ,(False,normalT $ ref unsigned)]
                                         },"tripleGetiOSVersion_")]
                        else [])++
                       [(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                   , ftName = "getArchName"
                                   },"tripleGetArchName")
                       ,(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                   , ftName = "getVendorName"
                                   },"tripleGetVendorName")
                       ,(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                   , ftName = "getOSName"
                                   },"tripleGetOSName")
                       ,(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                   , ftName = "getEnvironmentName"
                                   },"tripleGetEnvironmentName")]++
                       (if version>=llvm3_1
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isArch16Bit"
                                         },"tripleIsArch16Bit")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isArch32Bit"
                                         },"tripleIsArch32Bit")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isArch64Bit"
                                         },"tripleIsArch64Bit")]
                        else [])++
                       (if version>=llvm3_0
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isMacOSX"
                                         },"tripleIsMacOSX")]
                        else [])++
                       (if version>=llvm3_3
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isiOS"
                                         },"tripleIsiOS")]
                        else [])++
                       (if version>=llvm3_0
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isOSDarwin"
                                         },"tripleIsOSDarwin")]
                        else [])++
                       (if version>=llvm3_5
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isOSFreeBSD"
                                         },"tripleIsOSFreeBSD")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isWindowsMSVCEnvironment"
                                         },"tripleIsWindowsMSVCEnvironment")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isKnownWindowsMSVCEnvironment"
                                         },"tripleIsKnownWindowsMSVCEnvironment")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isWindowsCygwinEnvironment"
                                         },"tripleIsWindowsCygwinEnvironment")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isWindowsGNUEnvironment"
                                         },"tripleIsWindowsGNUEnvironment")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isOSCygMing"
                                         },"tripleIsOSCygMing")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isOSMSVCRT"
                                         },"tripleIsOSMSVCRT")]
                        else [])++
                       (if version>=llvm3_0
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isOSWindows"
                                         },"tripleIsOSWindows")]
                        else [])++
                       (if version>=llvm3_3
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isOSNaCl"
                                         },"tripleIsOSNaCl")]
                        else [])++
                       (if version>=llvm3_5
                        then [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isOSLinux"
                                         },"tripleIsOSLinux")]
                        else [])++
                       [(memberFun { ftName = "setArch"
                                   , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],
                                                                          ClassName "Triple" []] "ArchType")]
                                   },"tripleSetArch_")
                       ,(memberFun { ftName = "setVendor"
                                   , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],
                                                                          ClassName "Triple" []] "VendorType")]
                                   },"tripleSetVendor_")
                       ,(memberFun { ftName = "setOS"
                                   , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],
                                                                          ClassName "Triple" []] "OSType")]
                                   },"tripleSetOS_")]++
                       (if version>=llvm2_9
                        then [(memberFun { ftName = "setEnvironment"
                                         , ftArgs = [(False,normalT $
                                                            EnumType [ClassName "llvm" [],
                                                                      ClassName "Triple" []] "EnvironmentType")]
                                         },"tripleSetEnvironment_")]
                        else [])
          }
    ,Spec { specHeader = "llvm/ADT/Triple.h"
          , specNS = [ClassName "llvm" [],ClassName "Triple" []]
          , specName = "ArchType"
          , specTemplateArgs = []
          , specType = EnumSpec $
                       EnumNode "ArchType"
                       [Right $ EnumLeaf name ("Arch_"++name)
                       | name <- ["UnknownArch"
                                 ,"arm"]++
                                 (if version>=llvm3_5
                                  then ["armeb","arm64","arm64_be"]
                                  else [])++
                                 (if version<=llvm3_1
                                  then ["cellspu"]
                                  else [])++
                                 (if version>=llvm3_3
                                  then ["aarch64"]
                                  else [])++
                                 (if version>=llvm3_5
                                  then ["aarch64_be"]
                                  else [])++
                                 (if version>=llvm3_1
                                  then ["hexagon"]
                                  else [])++
                                 ["mips","mipsel"]++
                                 (if version>=llvm3_0
                                  then ["mips64","mips64el"]
                                  else [])++
                                 ["msp430"
                                 ,"ppc","ppc64"]++
                                 (if version>=llvm3_4
                                  then ["ppc64le"]
                                  else [])++
                                 (if version>=llvm3_1
                                  then ["r600"]
                                  else [])++
                                 ["sparc","sparcv9"]++
                                 (if version>=llvm3_3
                                  then ["systemz"]
                                  else [])++
                                 ["tce","thumb"]++
                                 (if version>=llvm3_5
                                  then ["thumbeb"]
                                  else [])++
                                 ["x86","x86_64","xcore"]++
                                 (if version<=llvm3_2
                                  then ["mblaze"]
                                  else [])++
                                 (if version<=llvm3_1
                                  then (if version>=llvm3_0
                                        then ["ptx32","ptx64"]
                                        else [])
                                  else ["nvptx","nvptx64"])++
                                 (if version>=llvm3_0
                                  then ["le32"]
                                  else [])++
                                 (if version>=llvm3_0
                                  then ["amdil"]
                                  else [])++
                                 (if version>=llvm3_2
                                  then ["spir","spir64"]
                                  else []) ]
          }
    ,Spec { specHeader = "llvm/ADT/Triple.h"
          , specNS = [ClassName "llvm" [],ClassName "Triple" []]
          , specName = "VendorType"
          , specTemplateArgs = []
          , specType = EnumSpec $
                       EnumNode "VendorType"
                       [Right $ EnumLeaf name ("Vendor_"++name)
                       | name <- ["UnknownVendor"
                                 ,"Apple","PC"]++
                                 (if version>=llvm3_0
                                  then ["SCEI"]
                                  else [])++
                                 (if version>=llvm3_1
                                  then ["BGP","BGQ"]
                                  else [])++
                                 (if version>=llvm3_2
                                  then ["Freescale","IBM"]
                                  else [])++
                                 (if version>=llvm3_4
                                  then ["NVIDIA"]
                                  else [])]
          }
    ,Spec { specHeader = "llvm/ADT/Triple.h"
          , specNS = [ClassName "llvm" [],ClassName "Triple" []]
          , specName = "OSType"
          , specTemplateArgs = []
          , specType = EnumSpec $
                       EnumNode "OSType"
                       [Right $ EnumLeaf name ("OS_"++name)
                       | name <- ["UnknownOS"
                                 ,"AuroraUX","Cygwin","Darwin","DragonFly","FreeBSD"]++
                                 (if version>=llvm3_0
                                  then ["IOS","KFreeBSD"]
                                  else [])++
                                 ["Linux","Lv2"]++
                                 (if version>=llvm3_0
                                  then ["MacOSX"]
                                  else [])++
                                 ["MinGW32"
                                 ,"NetBSD","OpenBSD","Solaris","Win32"
                                 ,"Haiku","Minix"]++
                                 (if version>=llvm3_0
                                  then ["RTEMS"]
                                  else [])++
                                 (if version>=llvm3_3
                                  then ["NaCl"]
                                  else [])++
                                 (if version>=llvm3_1
                                  then ["CNK"]
                                  else [])++
                                 (if version>=llvm3_2
                                  then ["Bitrig","AIX"]
                                  else [])++
                                 (if version>=llvm3_4
                                  then ["CUDA","NVCL"]
                                  else [])]
          }]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/ADT/Triple.h"
                , specNS = [ClassName "llvm" [],ClassName "Triple" []]
                , specName = "EnvironmentType"
                , specTemplateArgs = []
                , specType = EnumSpec $
                             EnumNode "EnvironmentType"
                             [Right $ EnumLeaf name ("Env_"++name)
                             | name <- ["UnknownEnvironment"
                                       ,"GNU","GNUEABI"]++
                                       (if version>=llvm3_1
                                        then ["GNUEABIHF"]
                                        else [])++
                                       (if version>=llvm3_3
                                        then ["GNUX32"]
                                        else [])++
                                       (if version>=llvm3_5
                                        then ["CODE16"]
                                        else [])++
                                       ["EABI","MachO"]++
                                       (if version>=llvm3_5
                                        then ["EABIHF"]
                                        else [])++
                                       (if version>=llvm3_2
                                        then ["Android","ELF"]
                                        else [])++
                                       (if version>=llvm3_5
                                        then ["MSVC","Itanium","Cygnus"]
                                        else [])]
                }]
     else [])++
    (if version>=llvm3_5
     then [Spec { specHeader = "llvm/ADT/Triple.h"
                , specNS = [ClassName "llvm" [],ClassName "Triple" []]
                , specName = "ObjectFormatType"
                , specTemplateArgs = []
                , specType = EnumSpec $
                             EnumNode "ObjectFormatType"
                             [Right $ EnumLeaf name ("ObjectFormat_"++name)
                             | name <- ["UnknownObjectFormat"
                                       ,"COFF","ELF","MachO"]]
                }]
     else [])++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/Target/TargetLibraryInfo.h"
                , specNS = [ClassName "llvm" [],ClassName "LibFunc" []]
                , specName = "Func"
                , specTemplateArgs = []
                , specType = EnumSpec $
                             EnumNode "LibFunc"
                             [Right $ EnumLeaf name ("Func_"++name)
                              | name <- ["fiprintf"
                                       ,"iprintf"
                                       ,"memcpy"
                                       ,"memset"
                                       ,"memset_pattern16"
                                       ,"siprintf"]++
                                       (if version>=llvm3_0
                                        then ["memmove"]
                                        else [])++
                                       (if version>=llvm3_1
                                        then ["cxa_atexit"
                                             ,"cxa_guard_abort"
                                             ,"cxa_guard_acquire"
                                             ,"cxa_guard_release"
                                             ,"acos"
                                             ,"acosf"
                                             ,"acosl"
                                             ,"asin"
                                             ,"asinf"
                                             ,"asinl"
                                             ,"atan"
                                             ,"atan2"
                                             ,"atan2f"
                                             ,"atan2l"
                                             ,"atanf"
                                             ,"atanl"
                                             ,"ceil"
                                             ,"ceilf"
                                             ,"ceill"
                                             ,"copysign"
                                             ,"copysignf"
                                             ,"copysignl"
                                             ,"cos"
                                             ,"cosf"
                                             ,"cosh"
                                             ,"coshf"
                                             ,"coshl"
                                             ,"cosl"
                                             ,"exp"
                                             ,"exp2"
                                             ,"exp2f"
                                             ,"exp2l"
                                             ,"expf"
                                             ,"expl"
                                             ,"expm1"
                                             ,"expm1f"
                                             ,"expm1l"
                                             ,"fabs"
                                             ,"fabsf"
                                             ,"fabsl"
                                             ,"floor"
                                             ,"floorf"
                                             ,"floorl"
                                             ,"fmod"
                                             ,"fmodf"
                                             ,"fmodl"
                                             ,"fputs"
                                             ,"fwrite"
                                             ,"log"
                                             ,"log10"
                                             ,"log10f"
                                             ,"log10l"
                                             ,"log1p"
                                             ,"log1pf"
                                             ,"log1pl"
                                             ,"log2"
                                             ,"log2f"
                                             ,"log2l"
                                             ,"logf"
                                             ,"logl"
                                             ,"nearbyint"
                                             ,"nearbyintf"
                                             ,"nearbyintl"
                                             ,"pow"
                                             ,"powf"
                                             ,"powl"
                                             ,"rint"
                                             ,"rintf"
                                             ,"rintl"
                                             ,"round"
                                             ,"roundf"
                                             ,"roundl"
                                             ,"sin"
                                             ,"sinf"
                                             ,"sinh"
                                             ,"sinhf"
                                             ,"sinhl"
                                             ,"sinl"
                                             ,"sqrt"
                                             ,"sqrtf"
                                             ,"sqrtl"
                                             ,"tan"
                                             ,"tanf"
                                             ,"tanh"
                                             ,"tanhf"
                                             ,"tanhl"
                                             ,"tanl"
                                             ,"trunc"
                                             ,"truncf"
                                             ,"truncl"]++
                                        (if version>=llvm3_3
                                         then ["under_IO_getc"
                                              ,"under_IO_putc"
                                              ,"ZdaPv"
                                              ,"ZdlPv"
                                              ,"Znaj"
                                              ,"ZnajRKSt9nothrow_t"
                                              ,"Znam"
                                              ,"ZnamRKSt9nothrow_t"
                                              ,"Znwj"
                                              ,"ZnwjRKSt9nothrow_t"
                                              ,"Znwm"
                                              ,"ZnwmRKSt9nothrow_t"
                                              ,"dunder_isoc99_scanf"
                                              ,"dunder_isoc99_sscanf"
                                              ,"memcpy_chk"
                                              ,"dunder_strdup"
                                              ,"dunder_strndup"
                                              ,"dunder_strtok_r"
                                              ,"abs"
                                              ,"access"
                                              ,"acosh"
                                              ,"acoshf"
                                              ,"acoshl"
                                              ,"asinh"
                                              ,"asinhf"
                                              ,"asinhl"
                                              ,"atanh"
                                              ,"atanhf"
                                              ,"atanhl"
                                              ,"atof"
                                              ,"atoi"
                                              ,"atol"
                                              ,"atoll"
                                              ,"bcmp"
                                              ,"bcopy"
                                              ,"bzero"
                                              ,"calloc"
                                              ,"cbrt"
                                              ,"cbrtf"
                                              ,"cbrtl"
                                              ,"chmod"
                                              ,"chown"
                                              ,"clearerr"
                                              ,"closedir"
                                              ,"ctermid"
                                              ,"exp10"
                                              ,"exp10f"
                                              ,"exp10l"
                                              ,"fclose"
                                              ,"fdopen"
                                              ,"feof"
                                              ,"ferror"
                                              ,"fflush"
                                              ,"ffs"
                                              ,"ffsl"
                                              ,"ffsll"
                                              ,"fgetc"
                                              ,"fgetpos"
                                              ,"fgets"
                                              ,"fileno"
                                              ,"flockfile"
                                              ,"fopen"
                                              ,"fopen64"
                                              ,"fprintf"
                                              ,"fputc"
                                              ,"fread"
                                              ,"free"
                                              ,"frexp"
                                              ,"frexpf"
                                              ,"frexpl"
                                              ,"fscanf"
                                              ,"fseek"
                                              ,"fseeko"
                                              ,"fseeko64"
                                              ,"fsetpos"
                                              ,"fstat"
                                              ,"fstat64"
                                              ,"fstatvfs"
                                              ,"fstatvfs64"
                                              ,"ftell"
                                              ,"ftello"
                                              ,"ftello64"
                                              ,"ftrylockfile"
                                              ,"funlockfile"
                                              ,"getc"
                                              ,"getc_unlocked"
                                              ,"getchar"
                                              ,"getenv"
                                              ,"getitimer"
                                              ,"getlogin_r"
                                              ,"getpwnam"
                                              ,"gets"
                                              ,"htonl"
                                              ,"htons"
                                              ,"isascii"
                                              ,"isdigit"
                                              ,"labs"
                                              ,"lchown"
                                              ,"llabs"
                                              ,"logb"
                                              ,"logbf"
                                              ,"logbl"
                                              ,"lstat"
                                              ,"lstat64"
                                              ,"malloc"
                                              ,"memalign"
                                              ,"memccpy"
                                              ,"memchr"
                                              ,"memcmp"
                                              ,"memrchr"
                                              ,"mkdir"
                                              ,"mktime"
                                              ,"modf"
                                              ,"modff"
                                              ,"modfl"
                                              ,"ntohl"
                                              ,"ntohs"
                                              ,"open"
                                              ,"open64"
                                              ,"opendir"
                                              ,"pclose"
                                              ,"perror"
                                              ,"popen"
                                              ,"posix_memalign"
                                              ,"pread"
                                              ,"printf"
                                              ,"putc"
                                              ,"putchar"
                                              ,"puts"
                                              ,"pwrite"
                                              ,"qsort"
                                              ,"read"
                                              ,"readlink"
                                              ,"realloc"
                                              ,"reallocf"
                                              ,"realpath"
                                              ,"remove"
                                              ,"rename"
                                              ,"rewind"
                                              ,"rmdir"
                                              ,"scanf"
                                              ,"setbuf"
                                              ,"setitimer"
                                              ,"setvbuf"
                                              ,"snprintf"
                                              ,"sprintf"
                                              ,"sscanf"
                                              ,"stat"
                                              ,"stat64"
                                              ,"statvfs"
                                              ,"statvfs64"
                                              ,"stpcpy"
                                              ,"stpncpy"
                                              ,"strcasecmp"
                                              ,"strcat"
                                              ,"strchr"
                                              ,"strcmp"
                                              ,"strcoll"
                                              ,"strcpy"
                                              ,"strcspn"
                                              ,"strdup"
                                              ,"strlen"
                                              ,"strncasecmp"
                                              ,"strncat"
                                              ,"strncmp"
                                              ,"strncpy"
                                              ,"strndup"
                                              ,"strnlen"
                                              ,"strpbrk"
                                              ,"strrchr"
                                              ,"strspn"
                                              ,"strstr"
                                              ,"strtod"
                                              ,"strtof"
                                              ,"strtok"
                                              ,"strtok_r"
                                              ,"strtol"
                                              ,"strtold"
                                              ,"strtoll"
                                              ,"strtoul"
                                              ,"strtoull"
                                              ,"strxfrm"
                                              ,"system"
                                              ,"times"
                                              ,"tmpfile"
                                              ,"tmpfile64"
                                              ,"toascii"
                                              ,"uname"
                                              ,"ungetc"
                                              ,"unlink"
                                              ,"unsetenv"
                                              ,"utime"
                                              ,"utimes"
                                              ,"valloc"
                                              ,"vfprintf"
                                              ,"vfscanf"
                                              ,"vprintf"
                                              ,"vscanf"
                                              ,"vsnprintf"
                                              ,"vsprintf"
                                              ,"vsscanf"
                                              ,"write"]
                                         else[])
                                        else [])
                             ]
                }
          ]
      else [])++
   (if version<llvm3_5
    then []
    else [Spec { specHeader = "llvm/IR/Dominators.h"
               , specNS = llvmNS
               , specName = "DominatorTreeWrapperPass"
               , specTemplateArgs = []
               , specType = classSpec
                            [(memberFun { ftReturnType = normalT $ ref $ llvmType "DominatorTree"
                                        , ftName = "getDomTree"
                                        },"dominatorTreeWrapperPassGetDomTree")]
               }])++
   [Spec { specHeader = if version<llvm3_5
                        then "llvm/Analysis/Dominators.h"
                        else "llvm/IR/Dominators.h"
         , specNS = llvmNS
         , specName = "DominatorTree"
         , specTemplateArgs = []
         , specType = classSpec $
                      [(Constructor [],"newDominatorTree")
                      ,(Destructor False,"deleteDominatorTree")
                      ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType "BasicBlock"] False
                                  , ftName = "getRootNode"
                                  },"dominatorTreeGetRootNode")
                      ,(memberFun { ftReturnType = normalT bool
                                  , ftName = "compare"
                                  , ftArgs = [(False,normalT $ ref $ llvmType "DominatorTree")]
                                  },"dominatorTreeCompare")
                      ,(memberFun { ftReturnType = normalT bool
                                  , ftName = "dominates"
                                  , ftArgs = [(False,(if version>=llvm2_9
                                                      then constT
                                                      else normalT) $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType "BasicBlock"] False)
                                             ,(False,(if version>=llvm2_9
                                                      then constT
                                                      else normalT) $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType "BasicBlock"] False)]
                                  },"dominatorTreeDominates")
                      ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                  , ftName = "findNearestCommonDominator"
                                  , ftArgs = [(False,normalT $ ptr $ llvmType "BasicBlock")
                                             ,(False,normalT $ ptr $ llvmType "BasicBlock")]
                                  },"dominatorTreeFindNearestCommonDominator")
                      ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType "BasicBlock"] False
                                  , ftName = "getNode"
                                  , ftArgs = [(False,normalT $ ptr $ llvmType "BasicBlock")]
                                  },"dominatorTreeGetNode")]++
                      (if version>=llvm3_5
                       then [(memberFun { ftName = "recalculate"
                                        , ftArgs = [(False,normalT $ ref $ llvmType "Function")]
                                        },"dominatorTreeRecalculate")]
                       else [])
         }]++
   [Spec { specHeader = if version<llvm3_5
                        then "llvm/Analysis/Dominators.h"
                        else "llvm/IR/Dominators.h"
         , specNS = llvmNS
         , specName = "DomTreeNodeBase"
         , specTemplateArgs = [normalT $ llvmType tp]
         , specType = classSpec
                      [(memberFun { ftReturnType = normalT $ ptr $ llvmType tp
                                  , ftName = "getBlock"
                                  },"domTreeNodeBaseGetBlock"++tp)
                      ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType tp] False
                                  , ftName = "getIDom"
                                  },"domTreeNodeBaseGetIDom"++tp)
                      ,(memberFun { ftReturnType = constT $ ref $ NamedType [ClassName "std" []] "vector" [normalT $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType tp] False] False
                                  , ftName = "getChildren"
                                  },"domTreeNodeBaseGetChildren"++tp)
                      ,(memberFun { ftReturnType = normalT bool
                                  , ftName = "compare"
                                  , ftArgs = [(False,normalT $ ptr $ NamedType llvmNS "DomTreeNodeBase" [normalT $ llvmType tp] False)]
                                  },"domTreeNodeBaseCompare"++tp)
                      ,(memberFun { ftReturnType = normalT unsigned
                                  , ftName = "getDFSNumIn"
                                  },"domTreeNodeBaseGetDFSNumIn"++tp)
                      ,(memberFun { ftReturnType = normalT unsigned
                                  , ftName = "getDFSNumOut"
                                  },"domTreeNodeBaseGetDFSNumOut"++tp)]
         }
    | tp <- ["BasicBlock"]]++
   [Spec { specHeader = if version<llvm3_5
                        then "llvm/Linker.h"
                        else "llvm/Linker/Linker.h"
         , specNS = llvmNS
         , specName = "Linker"
         , specTemplateArgs = []
         , specType = classSpec
                      [(Constructor $ (if version>=llvm3_3
                                       then []
                                       else [(False,normalT $ llvmType "StringRef")])++
                                      [(False,normalT $ ptr $ llvmType "Module")
                                      ],"newLinker")
                      ,(Destructor False,"deleteLinker")
                      ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Module"
                                  , ftName = "getModule"
                                  },"linkerGetModule")
                      ,(memberFun { ftReturnType = normalT bool
                                  , ftName = if version>=llvm3_3
                                             then "linkInModule"
                                             else "LinkInModule"
                                  , ftArgs = [(False,normalT $ ptr $ llvmType "Module")]++
                                             (if version>=llvm3_3
                                              then [(False,normalT unsigned)]
                                              else [])++
                                             [(False,normalT $ ptr $ NamedType [ClassName "std" []] "string" [] False)]
                                  },"linkerLinkInModule")]
         }
   ,if version>=llvm3_3
    then Spec { specHeader = irInclude version "Attributes.h"
              , specNS = llvmNS
              , specName = "AttributeSet"
              , specTemplateArgs = []
              , specType = classSpec
                           [(Constructor [],"newAttributeSet")
                           ,(Destructor False,"deleteAttributeSet")
                           ,(memberFun { ftReturnType = normalT $ llvmType "AttributeSet"
                                       , ftName = "addAttribute"
                                       , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                  ,(False,normalT unsigned)
                                                  ,(False,normalT $ EnumType [ClassName "llvm" []
                                                                             ,ClassName "Attribute" []] "AttrKind")
                                                  ]
                                       },"attributeSetAddAttribute_")
                           ,(memberFun { ftReturnType = normalT bool
                                       , ftName = "hasAttribute"
                                       , ftArgs = [(False,normalT unsigned)
                                                  ,(False,normalT $ EnumType [ClassName "llvm" []
                                                                             ,ClassName "Attribute" []] "AttrKind")]
                                       },"attributeSetHasAttribute_")
                           ,(memberFun { ftReturnType = normalT unsigned
                                       , ftName = "getParamAlignment"
                                       , ftArgs = [(False,normalT unsigned)]
                                       },"attributeSetGetParamAlignment_")
                           ,(memberFun { ftReturnType = normalT unsigned
                                       , ftName = "getStackAlignment"
                                       , ftArgs = [(False,normalT unsigned)]
                                       },"attributeSetGetStackAlignment_")]
              }
    else Spec { specHeader = irInclude version "Attributes.h"
              , specNS = llvmNS
              , specName = "Attributes"
              , specTemplateArgs = []
              , specType = classSpec $
                           [(Constructor [],"newAttributes")
                           ,(Destructor False,"deleteAttributes")]++
                           (if version<=llvm3_1
                            then (if version==llvm3_1
                                  then [(Constructor [(False,normalT uint64_t)],"newAttributesFromAttr_")
                                       ,(memberFun { ftReturnType = normalT $ llvmType "Attributes"
                                                   , ftName = "operator|"
                                                   , ftArgs = [(False,normalT $ ref $ llvmType "Attributes")]
                                                   },"attributesUnion")
                                       ,(memberFun { ftReturnType = normalT $ llvmType "Attributes"
                                                   , ftName = "operator&"
                                                   , ftArgs = [(False,normalT $ ref $ llvmType "Attributes")]
                                                   },"attributesIntersection")]
                                  else [])
                            else [(memberFun { ftReturnType = normalT $ llvmType "Attributes"
                                             , ftName = "get"
                                             , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                        ,(False,normalT $ ref $ llvmType "AttrBuilder")]
                                             , ftStatic = True
                                             },"attributesGet")])
              }]++
   (if version>=llvm3_3
    then []
    else [Spec { specHeader = irInclude version "Attributes.h"
               , specNS = llvmNS
               , specName = "AttrListPtr"
               , specTemplateArgs = []
               , specType = classSpec $
                            [(Constructor [],"newAttrListPtr")
                            ,(memberFun { ftReturnType = normalT $ llvmType "AttrListPtr"
                                        , ftName = "addAttr"
                                        , ftArgs = (if version>=llvm3_2
                                                    then [(False,normalT $ ref $ llvmType "LLVMContext")]
                                                    else [])++
                                                   [(False,normalT unsigned)
                                                   ,(False,normalT $ llvmType "Attributes")]
                                        },"attrListPtrAddAttr")]
               }])++
   (if version>=llvm3_2
    then [Spec { specHeader = irInclude version "Attributes.h"
               , specNS = llvmNS
               , specName = "AttrBuilder"
               , specTemplateArgs = []
               , specType = classSpec $
                            [(Constructor [],"newAttrBuilder")
                            ,(memberFun { ftReturnType = normalT $ ref $ llvmType "AttrBuilder"
                                        , ftName = "addAttribute"
                                        , ftArgs = [(False,normalT $ EnumType
                                                           [ClassName "llvm" []
                                                           ,ClassName (if version>=llvm3_3
                                                                       then "Attribute"
                                                                       else "Attributes") []]
                                                           (if version>=llvm3_3
                                                            then "AttrKind"
                                                            else "AttrVal"))]
                                       },"attrBuilderAdd")
                            ]
               }
         ,Spec { specHeader = irInclude version "Attributes.h"
               , specNS = [ClassName "llvm" []
                          ,ClassName (if version>=llvm3_3
                                      then "Attribute"
                                      else "Attributes") []]
               , specName = (if version>=llvm3_3
                             then "AttrKind"
                             else "AttrVal")
               , specTemplateArgs = []
               , specType = EnumSpec $
                            EnumNode (if version>=llvm3_3
                                      then "AttrKind"
                                      else "AttrVal") $
                            [ Right $ EnumLeaf name ("Attr"++name)
                            | name <- ["None"               -- No attributes have been set.
                                      ]++
                                      (if version<=llvm3_2
                                       then ["AddressSafety" -- Address safety checking is on.
                                            ]
                                       else [])++
                                      ["Alignment"          -- Alignment of parameter (5 bits) stored as log2 of alignment with +1 bias 0 means unaligned (different from align(1))
                                      ,"AlwaysInline"       -- inline=always
                                      ]++
                                      (if version>=llvm3_4
                                       then ["Builtin"            -- Callee is recognized as a builtin, despite nobuiltin attribute on its declaration.
                                            ]
                                       else [])++
                                      ["ByVal"              -- Pass structure by value.
                                      ]++
                                      (if version>=llvm3_5
                                       then ["InAlloca"           -- Pass structure in an alloca.
                                            ]
                                       else [])++
                                      (if version>=llvm3_4
                                       then ["Cold"               -- Marks function as being in a cold path.
                                            ]
                                       else [])++
                                      ["InlineHint"         -- Source said inlining was desirable.
                                      ,"InReg"              -- Force argument to be passed in register.
                                      ,"MinSize"            -- Function must be optimized for size first.
                                      ,"Naked"              -- Naked function.
                                      ,"Nest"               -- Nested function static chain.
                                      ,"NoAlias"            -- Considered to not alias after call.
                                      ]++
                                      (if version>=llvm3_3
                                       then ["NoBuiltin"          -- Callee isn't recognized as a builtin.
                                            ]
                                       else [])++
                                      ["NoCapture"          -- Function creates no aliases of pointer.
                                      ]++
                                      (if version>=llvm3_3
                                       then ["NoDuplicate"        -- Call cannot be duplicated.
                                            ]
                                       else [])++
                                      ["NoImplicitFloat"    -- Disable implicit floating point insts.
                                      ,"NoInline"           -- inline=never
                                      ,"NonLazyBind"        -- Function is called early and/or often, so lazy binding isn't worthwhile
                                      ]++
                                      (if version>=llvm3_5
                                       then ["NonNull"            -- Pointer is known to be not null.
                                            ]
                                       else [])++
                                      ["NoRedZone"          -- Disable redzone.
                                      ,"NoReturn"           -- Mark the function as not returning.
                                      ,"NoUnwind"           -- Function doesn't unwind stack.
                                      ,"OptimizeForSize"    -- opt_size
                                      ]++
                                      (if version>=llvm3_4
                                       then ["OptimizeNone"       -- Function must not be optimized.
                                            ]
                                       else [])++
                                      ["ReadNone"           -- Function does not access memory.
                                      ,"ReadOnly"           -- Function only reads from memory.
                                      ]++
                                      (if version>=llvm3_3
                                       then ["Returned"           -- Return value is always equal to this argument.
                                            ]
                                       else [])++
                                      ["ReturnsTwice"       -- Function can return twice.
                                      ,"SExt"               -- Sign extended before/after call.
                                      ,"StackAlignment"     -- Alignment of stack for function (3 bits) stored as log2 of alignment with +1 bias 0 means unaligned (different from alignstack=(1))
                                      ,"StackProtect"       -- Stack protection.
                                      ,"StackProtectReq"    -- Stack protection required.
                                      ]++
                                      (if version>=llvm3_3
                                       then ["StackProtectStrong" -- Strong Stack protection.
                                            ]
                                       else [])++
                                      ["StructRet"          -- Hidden pointer to structure to return.
                                      ]++
                                      (if version>=llvm3_3
                                       then ["SanitizeAddress"    -- AddressSanitizer is on.
                                            ,"SanitizeThread"     -- ThreadSanitizer is on.
                                            ,"SanitizeMemory"     -- MemorySanitizer is on.
                                            ]
                                       else [])++
                                      ["UWTable"            -- Function must be in a unwind table.
                                      ,"ZExt"               -- Zero extended before/after call.
                                      ]++
                                      (if version>=llvm3_3
                                       then ["EndAttrKinds"       -- Sentinal value useful for loops.
                                            ]
                                       else [])
                            ]
               }]
    else [])++
   [Spec { specHeader = "llvm/PassRegistry.h"
         , specNS = llvmNS
         , specName = "PassRegistry"
         , specTemplateArgs = []
         , specType = classSpec $
                      [(Constructor [],"newPassRegistry")
                      ,(Destructor False,"deletePassRegistry")
                      ,(memberFun { ftReturnType = constT $ ptr $ llvmType "PassInfo"
                                  , ftName = "getPassInfo"
                                  , ftArgs = [(False,constT $ ptr void)]
                                  },"passRegistryGetPassInfo_")
                      ,(memberFun { ftReturnType = constT $ ptr $ llvmType "PassInfo"
                                  , ftName = "getPassInfo"
                                  , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                  },"passRegistryGetPassInfoByName")
                      ,(memberFun { ftName = "registerPass"
                                  , ftArgs = [(False,constT $ ref $ llvmType "PassInfo")]++
                                             (if version>=llvm2_9
                                              then [(False,normalT bool)]
                                              else [])
                                  },"passRegistryRegisterPass")
                      ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "PassRegistry"
                                  , ftName = "getPassRegistry"
                                  , ftStatic = True
                                  },"passRegistryGet")]
         }]++
   (if version>=llvm3_6
    then [Spec { specHeader = irInclude version "Metadata.h"
               , specNS = llvmNS
               , specName = "AAMDNodes"
               , specTemplateArgs = []
               , specType = classSpecCustom "data AAMDNodes = AAMDNodes { aaMDNodesTBAA :: Ptr MDNode, aaMDNodesScope :: Ptr MDNode, aaMDNodesNoAlias :: Ptr MDNode }"
                            [(Constructor [(False,normalT $ ptr $ llvmType "MDNode")
                                          ,(False,normalT $ ptr $ llvmType "MDNode")
                                          ,(False,normalT $ ptr $ llvmType "MDNode")],
                              "newAAMDNodes")
                            ,(Getter "TBAA" (normalT $ ptr $ llvmType "MDNode") False,
                              "aaMDNodesGetTBAA")
                            ,(Setter "TBAA" (normalT $ ptr $ llvmType "MDNode"),
                              "aaMDNodesSetTBAA")
                            ,(Getter "Scope" (normalT $ ptr $ llvmType "MDNode") False,
                              "aaMDNodesGetScope")
                            ,(Setter "Scope" (normalT $ ptr $ llvmType "MDNode"),
                              "aaMDNodesSetScope")
                            ,(Getter "NoAlias" (normalT $ ptr $ llvmType "MDNode") False,
                              "aaMDNodesGetNoAlias")
                            ,(Setter "NoAlias" (normalT $ ptr $ llvmType "MDNode"),
                              "aaMDNodesSetNoAlias")
                            ,(SizeOf,"aaMDNodesSizeOf")
                            ,(AlignOf,"aaMDNodesAlignOf")]
               }]
    else [])++
   (if version>=llvm3_5
    then [Spec { specHeader = irInclude version "ValueHandle.h"
               , specNS = llvmNS
               , specName = "WeakVH"
               , specTemplateArgs = []
               , specType = classSpec $
                            [(Constructor [],"newWeakVHEmpty")
                            ,(Constructor [(True,normalT $ ptr $ llvmType "Value")],"newWeakVH_")]
               }]
    else [])++
   [Spec { specHeader = "llvm/Transforms/Utils/Cloning.h"
         , specNS = llvmNS
         , specName = "ClonedCodeInfo"
         , specTemplateArgs = []
         , specType = classSpecCustom
                      "data ClonedCodeInfo = ClonedCodeInfo { containsCalls :: Bool, containsDynamicAllocas :: Bool }"
                      [(Constructor [],"newClonedCodeInfo")
                      ,(Getter "ContainsCalls" (normalT bool) False,"clonedCodeInfoGetContainsCalls")
                      ,(Setter "ContainsCalls" (normalT bool),"clonedCodeInfoSetContainsCalls")
                      ,(Getter "ContainsDynamicAllocas" (normalT bool) False,
                        "clonedCodeInfoGetContainsDynamicAllocas")
                      ,(Setter "ContainsDynamicAllocas" (normalT bool),
                        "clonedCodeInfoSetContainsDynamicAllocas")
                      ,(SizeOf,"clonedCodeInfoSizeOf")
                      ,(AlignOf,"clonedCodeInfoAlignOf")]
         }
   ,Spec { specHeader = "llvm/Transforms/Utils/Cloning.h"
         , specNS = llvmNS
         , specName = "CloneBasicBlock"
         , specTemplateArgs = []
         , specType = GlobalFunSpec
                      (normalT $ ptr $ llvmType "BasicBlock")
                      [(False,constT $ ptr $ llvmType "BasicBlock")
                      ,(False,normalT $ ref $ NamedType llvmNS "ValueMap"
                                              [constT $ ptr $ llvmType "Value"
                                              ,normalT $ llvmType "WeakVH"] False)
                      ,(False,constT $ ref $ llvmType "Twine")
                      ,(False,normalT $ ptr $ llvmType "Function")
                      ,(False,normalT $ ptr $ llvmType "ClonedCodeInfo")]
                      "cloneBasicBlock"
         }]++
   [Spec { specHeader = irInclude version "ValueMap.h"
         , specNS = llvmNS
         , specName = "ValueMap"
         , specTemplateArgs = [keyT,valueT]
         , specType = classSpec $
                      [(Constructor [],"newValueMap"++name)
                      ,(Destructor False,"deleteValueMap"++name)
                      ,(memberFun { ftReturnType = normalT bool
                                  , ftName = "empty"
                                  },"valueMapEmpty"++name)]
         }
   | (keyT,valueT,name) <- [(constT $ ptr $ llvmType "Value",normalT $ llvmType "WeakVH","ValueToValue")] ]
