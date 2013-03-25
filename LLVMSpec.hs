module LLVMSpec where

import Generator
import CPPType

llvm :: [ClassSpec]
llvm = [ClassSpec { cspecHeader = "llvm/ADT/StringRef.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "StringRef"
                  , cspecTemplateArgs = []
                  , cspecFunctions 
                      = [(Constructor { ftConArgs = [] },GenHS,"newStringRefEmpty")
                        ,(Constructor { ftConArgs = [(False,constT cstring)] },GenHS,"newStringRef_")
                        ,(Destructor False,GenHS,"deleteStringRef")
                        ,(memberFun { ftReturnType = constT cstring
                                    , ftName = "data"
                                    },GenHS,"stringRefData_")
                        ,(memberFun { ftReturnType = normalT size_t
                                    , ftName = "size"
                                    },GenHS,"stringRefSize_")
                        ]
                  }
       ]++
       [ClassSpec { cspecHeader = "llvm/ADT/OwningPtr.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "OwningPtr"
                  , cspecTemplateArgs = [rtp]
                  , cspecFunctions = [(Constructor { ftConArgs = [(False,toPtr rtp)] },GenHS,"newOwningPtr"++tp)
                                     ,(Destructor False,GenHS,"deleteOwningPtr"++tp)
                                     ,(memberFun { ftReturnType = toPtr rtp
                                                 , ftName = "take"
                                                 },GenHS,"takeOwningPtr"++tp)]
                  }
       | tp <- ["MemoryBuffer"]
       , let rtp = Type [] (NamedType ["llvm"] tp [])
       ]++
       [ClassSpec { cspecHeader = "llvm/ADT/ArrayRef.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ArrayRef"
                  , cspecTemplateArgs = [rtp]
                  , cspecFunctions = [(Constructor { ftConArgs = [] },GenHS,"newArrayRefEmpty"++tp)
                                     ,(Constructor { ftConArgs = [(False,toPtr rtp)
                                                                 ,(False,normalT size_t)] },GenHS,"newArrayRef"++tp)
                                     ,(memberFun { ftReturnType = normalT size_t
                                                 , ftName = "size"
                                                 },GenHS,"arrayRefSize"++tp)
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "equals"
                                                 , ftArgs = [(False,normalT $ NamedType ["llvm"] "ArrayRef" [rtp])]
                                                 },GenHS,"arrayRefEquals"++tp)
                                     ,(memberFun { ftReturnType = toConstRef rtp
                                                 , ftName = "operator[]"
                                                 , ftArgs = [(False,normalT size_t)]
                                                 },GenHS,"arrayRefIndex"++tp)
                                     ]
                  }
       | tp <- ["Type"]
       , let rtp = Type [] (ptr $ NamedType ["llvm"] tp [])
       ]++
       concat [[ClassSpec { cspecHeader = "llvm/ADT/ilist.h"
                          , cspecNS = ["llvm"]
                          , cspecClassName = "iplist"
                          , cspecTemplateArgs = [rtp]
                          , cspecFunctions = [(Constructor { ftConArgs = [] },GenHS,"new"++tp++"List")
                                             ,(Destructor False,GenHS,"delete"++tp++"List")
                                             ,(memberFun { ftReturnType = normalT size_t
                                                         , ftName = "size"
                                                         },GenHS,"list"++tp++"Size")
                                             ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "ilist_iterator" [rtp]
                                                         , ftName = "begin"
                                                         },GenHS,"list"++tp++"Begin")
                                             ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "ilist_iterator" [rtp]
                                                         , ftName = "end"
                                                         },GenHS,"list"++tp++"End")
                                             ]
                          }
               ,ClassSpec { cspecHeader = "llvm/ADT/ilist.h"
                          , cspecNS = ["llvm"]
                          , cspecClassName = "ilist_iterator"
                          , cspecTemplateArgs = [rtp]
                          , cspecFunctions = [(memberFun { ftReturnType = toPtr rtp
                                                         , ftName = "operator->"
                                                         },GenHS,"listIterator"++tp++"Deref")
                                             ,(memberFun { ftReturnType = normalT $ RefType $ 
                                                                          NamedType ["llvm"] "ilist_iterator" [rtp]
                                                         , ftName = "operator++"
                                                         },GenHS,"listIterator"++tp++"Next")
                                             ,(memberFun { ftReturnType = normalT bool
                                                         , ftName = "operator=="
                                                         , ftArgs = [(False,constT $ RefType $ 
                                                                            NamedType ["llvm"] "ilist_iterator" [rtp])] 
                                                         },GenHS,"listIterator"++tp++"Eq")
                                             ,(memberFun { ftReturnType = normalT bool
                                                         , ftName = "operator!="
                                                         , ftArgs = [(False,constT $ RefType $ 
                                                                            NamedType ["llvm"] "ilist_iterator" [rtp])] 
                                                         },GenHS,"listIterator"++tp++"NEq")]
                          }
               ]
              | tp <- ["Function","Instruction","BasicBlock"]
              , let rtp = Type [] (NamedType ["llvm"] tp [])
              ]++
       [ClassSpec { cspecHeader = "llvm/ADT/APFloat.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "APFloat"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT double
                                                 , ftName = "convertToDouble"
                                                 },GenHS,"apFloatConvertToDouble")]
                  }
       ,ClassSpec { cspecHeader = "llvm/ADT/APInt.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "APInt"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getBitWidth"
                                                 },GenHS,"apIntGetBitWidth_")
                                     ,(memberFun { ftReturnType = normalT uint64_t
                                                 , ftName = "getZExtValue"
                                                 },GenHS,"apIntGetZExtValue")
                                     ,(memberFun { ftReturnType = normalT int64_t
                                                 , ftName = "getSExtValue"
                                                 },GenHS,"apIntGetSExtValue")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Support/DebugLoc.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "DebugLoc"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(Constructor { ftConArgs = [] },GenHS,"newDebugLoc")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isUnknown"
                                                 },GenHS,"debugLocIsUnknown")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getLine"
                                                 },GenHS,"debugLocGetLine")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getCol"
                                                 },GenHS,"debugLocGetCol")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "MDNode" []
                                                 , ftName = "getScope"
                                                 , ftArgs = [(False,constT $ ref $ NamedType ["llvm"] "LLVMContext" [])]
                                                 },GenHS,"debugLocGetScope")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "MDNode" []
                                                 , ftName = "getInlinedAt"
                                                 , ftArgs = [(False,constT $ ref $ NamedType ["llvm"] "LLVMContext" [])]
                                                 },GenHS,"debugLocGetInlinedAt")
                                     ,(memberFun { ftName = "dump" 
                                                 , ftArgs = [(False,constT $ ref $ NamedType ["llvm"] "LLVMContext" [])]
                                                 },GenHS,"debugLocDump")
                                     ]
                  }
       ]++
       [ClassSpec { cspecHeader = "llvm/Type.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Type"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT void
                                                 , ftName = "dump"
                                                 , ftOverloaded = True
                                                 },GenHS,"typeDump_")]++
                                     [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "is"++tp++"Ty"
                                                 , ftOverloaded = True
                                                 , ftPure = True
                                                 },GenHS,"is"++tp++"Ty_")
                                     | tp <- ["Void","Half","Float","Double","X86_FP80","FP128","PPC_FP128"
                                             ,"FloatingPoint","X86_MMX","Label"]
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "IntegerType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getBitWidth"
                                                 },GenHS,"getBitWidth_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "IntegerType" [] 
                                                 , ftName = "get"
                                                 , ftArgs = [(False,normalT $ RefType $ NamedType ["llvm"] "LLVMContext" [])
                                                            ,(False,normalT $ NamedType [] "unsigned" [])]
                                                 , ftStatic = True
                                                 },GenHS,"getIntegerType_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "CompositeType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
                                                 , ftName = "getTypeAtIndex"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 , ftOverloaded = True
                                                 },GenHS,"compositeTypeGetTypeAtIndex_")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "indexValid"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 , ftOverloaded = True
                                                 },GenHS,"compositeTypeIndexValid_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "SequentialType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
                                                 , ftName = "getElementType"
                                                 , ftOverloaded = True
                                                 },GenHS,"sequentialTypeGetElementType_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ArrayType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT uint64_t
                                                 , ftName = "getNumElements"
                                                 },GenHS,"arrayTypeGetNumElements_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "PointerType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getAddressSpace"
                                                 },GenHS,"pointerTypeGetAddressSpace_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                                 , ftName = "get"
                                                 , ftArgs = [(True,normalT $ ptr $ NamedType ["llvm"] "Type" [])
                                                            ,(False,normalT unsigned)]
                                                 , ftStatic = True
                                                 },GenHS,"pointerTypeGet_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "VectorType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getNumElements"
                                                 },GenHS,"vectorTypeGetNumElements_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                                 , ftName = "get"
                                                 , ftArgs = [(True,normalT $ ptr $ NamedType ["llvm"] "Type" [])
                                                            ,(False,normalT unsigned)]
                                                 , ftStatic = True
                                                 },GenHS,"vectorTypeGet_")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "StructType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isPacked"
                                                 },GenHS,"structTypeIsPacked")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "hasName"
                                                 },GenHS,"structTypeHasName")
                                     ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "StringRef" []
                                                 , ftName = "getName"
                                                 },GenHS,"structTypeGetName")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getNumElements"
                                                 },GenHS,"structTypeGetNumElements_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
                                                 , ftName = "getElementType"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 },GenHS,"structTypeGetElementType_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/DerivedTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "FunctionType"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isVarArg"
                                                 },GenHS,"functionTypeIsVarArg")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getNumParams"
                                                 },GenHS,"functionTypeGetNumParams_")
                                     ,(memberFun { ftReturnType = normalT (ptr $ NamedType ["llvm"] "Type" [])
                                                 , ftName = "getParamType"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 },GenHS,"functionTypeGetParamType_")
                                     ,(memberFun { ftReturnType = normalT (ptr $ NamedType ["llvm"] "Type" [])
                                                 , ftName = "getReturnType"
                                                 },GenHS,"functionTypeGetReturnType")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Value.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Value"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(Destructor True,GenHS,"deleteValue_")
                                     ,(memberFun { ftName = "dump"
                                                 , ftOverloaded = True
                                                 },GenHS,"valueDump_")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "hasName"
                                                 , ftOverloaded = True
                                                 },GenHS,"hasName_")
                                     ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "StringRef" []
                                                 , ftName = "getName"
                                                 , ftOverloaded = True
                                                 },GenHS,"getName_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
                                                 , ftName = "getType"
                                                 , ftOverloaded = True
                                                 },GenHS,"valueGetType_")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Argument.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Argument"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/BasicBlock.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "BasicBlock"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Function" []
                                                 , ftName = "getParent"
                                                 },GenHS,"basicBlockGetParent")
                                     ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "iplist" 
                                                                  [normalT $ NamedType ["llvm"] "Instruction" []]
                                                 , ftName = "getInstList"
                                                 },GenHS,"getInstList")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "TerminatorInst" []
                                                 , ftName = "getTerminator"
                                                 },GenHS,"getTerminator")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/InlineAsm.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "InlineAsm"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Metadata.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "MDNode"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Metadata.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "MDString"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/CodeGen/PseudoSourceValue.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "FixedStackPseudoSourceValue"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Constant.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Constant"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "BlockAddress"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                                 , ftName = "getAggregateElement"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 , ftOverloaded = True
                                                 },GenHS,"constantGetAggregateElement_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantArray"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "ArrayType" []
                                                 , ftName = "getType"
                                                 },GenHS,"constantArrayGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantDataSequential"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "SequentialType" []
                                                 , ftName = "getType"
                                                 , ftOverloaded = True
                                                 },GenHS,"constantDataSequentialGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantDataArray"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "ArrayType" []
                                                 , ftName = "getType"
                                                 },GenHS,"constantDataArrayGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantDataVector"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                                 , ftName = "getType"
                                                 },GenHS,"constantDataVectorGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantExpr"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [] --[(memberFun { ftReturnType = normalT $ ptr $ llvmType "Instruction"
                                        --            , ftName = "getAsInstruction"
                                        --            },GenHS,"constantExprGetAsInstruction")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantFP"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = constT $ ref $ llvmType "APFloat"
                                                 , ftName = "getValueAPF"
                                                 },GenHS,"constantFPGetValueAPF")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantInt"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "IntegerType" []
                                                 , ftName = "getType"
                                                 },GenHS,"constantIntGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantPointerNull"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                                 , ftName = "getType"
                                                 },GenHS,"constantPointerNullGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantStruct"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "StructType" []
                                                 , ftName = "getType"
                                                 },GenHS,"constantStructGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantVector"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                                 , ftName = "getType"
                                                 },GenHS,"constantVectorGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/GlobalValue.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "GlobalValue"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                                 , ftName = "getType"
                                                 , ftOverloaded = True
                                                 },GenHS,"globalValueGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/GlobalValue.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "GlobalAlias"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/GlobalValue.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "GlobalVariable"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isConstant"
                                                 },GenHS,"globalVariableIsConstant")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isThreadLocal"
                                                 },GenHS,"globalVariableIsThreadLocal")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                                 , ftName = "getInitializer"
                                                 },GenHS,"globalVariableGetInitializer")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Function.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Function"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isVarArg"
                                                 },GenHS,"functionIsVarArg")
                                     ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "iplist" 
                                                                  [normalT $ NamedType ["llvm"] "BasicBlock" []]
                                                 , ftName = "getBasicBlockList"
                                                 },GenHS,"getBasicBlockList")
                                     ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "BasicBlock" []
                                                 , ftName = "getEntryBlock"
                                                 },GenHS,"getEntryBlock")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "UndefValue"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Support/system_error.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "error_code"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(Destructor False,GenHS,"deleteErrorCode")
                                     ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                                 , ftName = "value"
                                                 },GenHS,"errorCodeValue_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Support/MemoryBuffer.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "MemoryBuffer"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(Destructor False,GenHS,"deleteMemoryBuffer")
                                     ,(memberFun { ftReturnType = normalT size_t 
                                                 , ftName = "getBufferSize"
                                                 },GenHS,"getBufferSize_")
                                     ,(memberFun { ftReturnType = normalT (NamedType ["llvm"] "error_code" []) 
                                                 , ftName = "getFile"
                                                 , ftArgs = [(False,normalT (NamedType ["llvm"] "StringRef" []))
                                                            ,(False,normalT (RefType $ NamedType ["llvm"] "OwningPtr" 
                                                                             [normalT (NamedType ["llvm"] "MemoryBuffer" [])]))
                                                            ,(False,normalT (NamedType [] "int64_t" []))
                                                            ,(False,normalT (NamedType [] "bool" []))]
                                                 , ftStatic = True
                                                 },GenHS,"getFileMemoryBuffer")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Support/SourceMgr.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "SMDiagnostic"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(Constructor [],GenHS,"newSMDiagnostic")
                                     ,(Destructor False,GenHS,"deleteSMDiagnostic")
                                     ,(memberFun { ftReturnType = normalT (NamedType ["llvm"] "StringRef" [])
                                                 , ftName = "getFilename"
                                                 },GenHS,"getFilename")
                                     ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                                 , ftName = "getLineNo"
                                                 },GenHS,"getLineNo_")
                                     ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                                 , ftName = "getColumnNo"
                                                 },GenHS,"getColumnNo_")
                                      ]
                  }
       ,ClassSpec { cspecHeader = "llvm/LLVMContext.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "LLVMContext"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(Constructor [],GenHS,"newLLVMContext")
                                     ,(Destructor False,GenHS,"deleteLLVMContext")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Module.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Module"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(Constructor [(False,normalT $ NamedType ["llvm"] "StringRef" [])
                                                   ,(False,normalT $ RefType $ NamedType ["llvm"] "LLVMContext" [])
                                                   ],GenHS,"newModule")
                                     ,(Destructor False,GenHS,"deleteModule")
                                     ,(memberFun { ftReturnType = normalT void
                                                 , ftName = "dump"
                                                 },GenHS,"moduleDump")
                                     ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "iplist"
                                                                  [normalT $ NamedType ["llvm"] "Function" []] 
                                                 , ftName = "getFunctionList"
                                                 },GenHS,"getFunctionList") ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Support/IRReader.h"
                  , cspecNS = []
                  , cspecClassName = "llvm"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT (PtrType $ NamedType ["llvm"] "Module" []) 
                                                 , ftName = "ParseIR"
                                                 , ftArgs = [(False,normalT (PtrType $ NamedType ["llvm"] "MemoryBuffer" []))
                                                            ,(False,normalT (RefType $ NamedType ["llvm"] "SMDiagnostic" []))
                                                            ,(False,normalT (RefType $ NamedType ["llvm"] "LLVMContext" []))]
                                                 , ftStatic = True
                                                 },GenHS,"parseIR")]++
                                     [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isa"
                                                 , ftTemplArgs = [to_tp,from_tp]
                                                 , ftArgs = [(True,toConstRef from_tp)]
                                                 , ftStatic = True
                                                 , ftOverloaded = True
                                                 , ftPure = True
                                                 },GenHS,"isA"++to)
                                     | (to,from) <- [(to',"Value")
                                                    | to' <- ["Value"
                                                             ,"Argument"
                                                             ,"BasicBlock"
                                                             ,"InlineAsm"
                                                             ,"MDNode"
                                                             ,"MDString"
                                                             ,"PseudoSourceValue"
                                                             ,"User"
                                                             ,"FixedStackPseudoSourceValue"
                                                             ,"Constant"
                                                             ,"BlockAddress"
                                                             ,"ConstantAggregateZero"
                                                             ,"ConstantArray"
                                                             ,"ConstantDataSequential"
                                                             ,"ConstantDataArray"
                                                             ,"ConstantDataVector"
                                                             ,"ConstantExpr"
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
                                                             ,"Instruction"
                                                             ,"AtomicCmpXchgInst"
                                                             ,"AtomicRMWInst"
                                                             ,"BinaryOperator"
                                                             ,"CallInst"
                                                             ,"CmpInst"
                                                             ,"FCmpInst"
                                                             ,"ICmpInst"
                                                             ,"ExtractElementInst"
                                                             ,"FenceInst"
                                                             ,"GetElementPtrInst"
                                                             ,"InsertElementInst"
                                                             ,"InsertValueInst"
                                                             ,"LandingPadInst"
                                                             ,"PHINode"
                                                             ,"SelectInst"
                                                             ,"ShuffleVectorInst"
                                                             ,"StoreInst"
                                                             ,"TerminatorInst"
                                                             ,"BranchInst"
                                                             ,"IndirectBrInst"
                                                             ,"InvokeInst"
                                                             ,"ResumeInst"
                                                             ,"ReturnInst"
                                                             ,"SwitchInst"
                                                             ,"UnreachableInst"
                                                             ,"UnaryInstruction"
                                                             ,"AllocaInst"
                                                             ,"CastInst"
                                                             ,"BitCastInst"
                                                             ,"FPExtInst"
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
                                                              ,"IntegerType"]]
                                     , let to_tp = normalT $ NamedType ["llvm"] to []
                                           from_tp = normalT $ NamedType ["llvm"] from []
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/CodeGen/PseudoSourceValue.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "PseudoSourceValue"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Constants.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ConstantAggregateZero"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instruction.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Instruction"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "BasicBlock" []
                                                 , ftName = "getParent"
                                                 },GenHS,"instructionGetParent")
                                     ,(memberFun { ftReturnType = constT $ ref $ NamedType ["llvm"] "DebugLoc" []
                                                 , ftName = "getDebugLoc"
                                                 },GenHS,"instructionGetDebugLoc")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "AtomicCmpXchgInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "AtomicRMWInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "BinaryOperator"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT int
                                                 , ftName = "getOpcode"
                                                 },GenHS,"binOpGetOpCode_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "CallInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isTailCall"
                                                 },GenHS,"callInstIsTailCall")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getNumArgOperands"
                                                 },GenHS,"callInstGetNumArgOperands_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                                 , ftName = "getArgOperand"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 },GenHS,"callInstGetArgOperand_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                                 , ftName = "getCalledValue"
                                                 },GenHS,"callInstGetCalledValue")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "CmpInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT int
                                                 , ftName = "getPredicate" 
                                                 , ftOverloaded = True
                                                 },GenHS,"cmpInstGetPredicate_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "FCmpInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ICmpInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ExtractElementInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                                 , ftName = "getVectorOperand"
                                                 },GenHS,"extractElementInstGetVectorOperand")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                                 , ftName = "getIndexOperand"
                                                 },GenHS,"extractElementInstGetIndexOperand")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "FenceInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "GetElementPtrInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                                 , ftName = "getType"
                                                 },GenHS,"getElementPtrInstGetType")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isInBounds"
                                                 },GenHS,"getElementPtrInstIsInBounds")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getPointerOperand"
                                                 },GenHS,"getElementPtrInstGetPointerOperand")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                                 , ftName = "idx_begin"
                                                 },GenHS,"getElementPtrInstIdxBegin")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                                 , ftName = "idx_end"
                                                 },GenHS,"getElementPtrInstIdxEnd")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "InsertElementInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                                 , ftName = "getType"
                                                 },GenHS,"insertElementInstGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "InsertValueInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "LandingPadInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "PHINode"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getNumIncomingValues"
                                                 },GenHS,"phiNodeGetNumIncomingValues_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                                 , ftName = "getIncomingValue"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 },GenHS,"phiNodeGetIncomingValue_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "BasicBlock" []
                                                 , ftName = "getIncomingBlock"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 },GenHS,"phiNodeGetIncomingBlock_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "SelectInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getCondition"
                                                 },GenHS,"selectInstGetCondition")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getTrueValue"
                                                 },GenHS,"selectInstGetTrueValue")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getFalseValue"
                                                 },GenHS,"selectInstGetFalseValue")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ShuffleVectorInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                                 , ftName = "getType"
                                                 },GenHS,"shuffleVectorInstGetType")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "StoreInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isVolatile"
                                                 },GenHS,"storeInstIsVolatile")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getAlignment"
                                                 },GenHS,"storeInstGetAlignment_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getValueOperand"
                                                 },GenHS,"storeInstGetValueOperand")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getPointerOperand"
                                                 },GenHS,"storeInstGetPointerOperand")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "TerminatorInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getNumSuccessors"
                                                 , ftOverloaded = True
                                                 },GenHS,"terminatorInstGetNumSuccessors_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                                 , ftName = "getSuccessor"
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 , ftOverloaded = True
                                                 },GenHS,"terminatorInstGetSuccessor_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "BranchInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isConditional"
                                                 },GenHS,"branchInstIsConditional")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getCondition"
                                                 },GenHS,"branchInstGetCondition")]
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "IndirectBrInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "InvokeInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ResumeInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ReturnInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getReturnValue"
                                                 },GenHS,"returnInstGetReturnValue")]
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "SwitchInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getCondition"
                                                 },GenHS,"switchInstGetCondition")
                                     ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                                 , ftName = "case_begin"
                                                 },GenHS,"switchInstCaseBegin")
                                     ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                                 , ftName = "case_end"
                                                 },GenHS,"switchInstCaseEnd")
                                     ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                                 , ftName = "case_default"
                                                 },GenHS,"switchInstCaseDefault")]
                  }
       ,ClassSpec { cspecHeader = "llvm/InstrTypes.h"
                  , cspecNS = ["llvm","SwitchInst"]
                  , cspecClassName = "CaseIt"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                                 , ftName = "operator++"
                                                 },GenHS,"caseItNext")
                                     ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                                 , ftName = "operator--"
                                                 },GenHS,"caseItPrev")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "operator=="
                                                 , ftArgs = [(False,constT $ ref $ NamedType ["llvm","SwitchInst"] "CaseIt" [])]
                                                 },GenHS,"caseItEq")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ConstantInt"
                                                 , ftName = "getCaseValue"
                                                 },GenHS,"caseItGetCaseValue")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                                 , ftName = "getCaseSuccessor"
                                                 },GenHS,"caseItGetCaseSuccessor")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "UnreachableInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "UnaryInstruction"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "AllocaInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                                 , ftName = "getType"
                                                 },GenHS,"allocaInstGetType")
                                     ,(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isArrayAllocation"
                                                 },GenHS,"allocaInstIsArrayAllocation")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getArraySize"
                                                 },GenHS,"allocaInstGetArraySize")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getAlignment"
                                                 },GenHS,"allocaInstGetAlignment_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "CastInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "BitCastInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "FPExtInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "FPToUIInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "FPTruncInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "IntToPtrInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "PtrToIntInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "SExtInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "SIToFPInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "TruncInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "UIToFPInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ZExtInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "ExtractValueInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "LoadInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT bool
                                                 , ftName = "isVolatile"
                                                 },GenHS,"loadInstIsVolatile")
                                     ,(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getAlignment"
                                                 },GenHS,"loadInstGetAlignment_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "getPointerOperand"
                                                 },GenHS,"loadInstGetPointerOperand")
                                     ]
                  }
       ,ClassSpec { cspecHeader = "llvm/Instructions.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "VAArgInst"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/User.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "User"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT unsigned
                                                 , ftName = "getNumOperands" 
                                                 , ftOverloaded = True
                                                 },GenHS,"getNumOperands_")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                                 , ftName = "getOperand" 
                                                 , ftArgs = [(False,normalT unsigned)]
                                                 , ftOverloaded = True
                                                 },GenHS,"getOperand_")]
                  }
       ,ClassSpec { cspecHeader = "llvm/Operator.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Operator"
                  , cspecTemplateArgs = []
                  , cspecFunctions = []
                  }
       ,ClassSpec { cspecHeader = "llvm/Use.h"
                  , cspecNS = ["llvm"]
                  , cspecClassName = "Use"
                  , cspecTemplateArgs = []
                  , cspecFunctions = [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                                 , ftName = "get"
                                                 },GenHS,"useGet")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                                 , ftName = "getNext"
                                                 },GenHS,"useGetNext")
                                     ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "User"
                                                 , ftName = "getUser"
                                                 },GenHS,"useGetUser")]

                  }
       ]