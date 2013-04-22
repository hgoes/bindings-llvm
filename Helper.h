#define _TO_STRING(x) #x

#define TYPE_LEAF(name)

#define TYPE(name)\
TYPE_LEAF(name) ;\
\
class name##C a ;\
\
instance name##C name

#define SUBTYPE(super,sub)\
instance Subtype super sub where {\
  isA _ = isA##sub\
} ;\
instance super##C sub

#define SUBTYPE2(super1,super2,sub)\
SUBTYPE(super1,sub) ;\
SUBTYPE(super2,sub)

#define SUBTYPE3(super1,super2,super3,sub)\
SUBTYPE(super1,sub) ;\
SUBTYPE(super2,sub) ;\
SUBTYPE(super3,sub)

#define SUBTYPE4(super1,super2,super3,super4,sub)\
SUBTYPE(super1,sub);\
SUBTYPE(super2,sub);\
SUBTYPE(super3,sub);\
SUBTYPE(super4,sub)

#define SUBTYPE5(super1,super2,super3,super4,super5,sub)        \
SUBTYPE(super1,sub);\
SUBTYPE(super2,sub);\
SUBTYPE(super3,sub);\
SUBTYPE(super4,sub);\
SUBTYPE(super5,sub)

#define SPECIALIZE_IPLIST(name,cconv)\
instance IPListC name where {\
  ipListSize' = list##name##Size ;\
  ipListBegin = list##name##Begin ;\
  ipListEnd = list##name##End\
} ;\
\
instance IListIteratorC name where {\
  iListIteratorDeref = listIterator##name##Deref ;\
  iListIteratorNext = listIterator##name##Next ;\
  iListIteratorEq = listIterator##name##Eq ;\
  iListIteratorNEq = listIterator##name##NEq\
}

#if HS_LLVM_VERSION>=300
#define SPECIALIZE_ARRAYREF(name)\
instance ArrayRefC (Ptr name) where {\
  newArrayRef' = newArrayRef##name ;\
  newArrayRefEmpty = newArrayRefEmpty##name ;\
  arrayRefSize' = arrayRefSize##name ;\
  arrayRefEquals = arrayRefEquals##name ;\
  arrayRefIndex' = arrayRefIndex##name ;\
  deleteArrayRef = deleteArrayRef##name\
}
#else
#define SPECIALIZE_ARRAYREF(name)\
instance ArrayRefC (Ptr name) where {\
  newArrayRef' = newArrayRef##name ;\
  newArrayRefEmpty = newArrayRefEmpty##name ;\
  arrayRefSize' = arrayRefSize##name ;\
  arrayRefIndex' = arrayRefIndex##name ;\
  deleteArrayRef = deleteArrayRef##name\
}
#endif

#define SPECIALIZE_SETVECTOR(name)\
instance SetVectorC (Ptr name) where {\
  setVectorEmpty = setVector##name##Empty ;\
  setVectorBegin = setVector##name##Begin ;\
  setVectorEnd = setVector##name##End\
}

#define SPECIALIZE_VECTOR(name)\
instance VectorC (Ptr name) where {\
  vectorBegin = vector##name##Begin ;\
  vectorEnd = vector##name##End ;\
  vectorIteratorDeref = vectorIterator##name##Deref ;\
  vectorIteratorNext = vectorIterator##name##Next ;\
  vectorIteratorEq = vectorIterator##name##Eq\
}

#define SPECIALIZE_SMALLVECTOR(name)\
instance SmallVectorC (Ptr name) where {\
  newSmallVector = newSmallVector##name ;\
  deleteSmallVector = deleteSmallVector##name ;\
  smallVectorSize = smallVectorSize##name ;\
  smallVectorData = smallVectorData##name\
}

#define SPECIALIZE_PAIR(name1,name2)\
instance PairC (Ptr name1) (Ptr name2) where {\
  pairSize _ = sizeofPair##name1##_##name2 ;\
  pairFirst = pairFirst##name1##_##name2 ;\
  pairSecond = pairSecond##name1##_##name2\
}

#define FUN(cls,name,sig)\
foreign import capi unsafe _TO_STRING(llvm_proxy.h cls##_##name)\
  _##name :: Ptr t -> sig ;\
\
name :: cls##C t => Ptr t -> sig ;\
name = _##name

#define FUN_LEAF(cls,name,sig)\
foreign import capi unsafe _TO_STRING(llvm_proxy.h cls##_##name)\
  name :: Ptr cls -> sig

#define GETTYPE(cls)\
instance GetType cls where {\
  type TypeOfValue cls = Type ;\
  getType = valueGetType\
}

#define SPECIALIZE_OWNINGPTR(name,cconv)\
instance OwningPtrC name where {\
  newOwningPtr = newOwningPtr##name ;\
  deleteOwningPtr = deleteOwningPtr##name ;\
  takeOwningPtr = takeOwningPtr##name\
}
#define PRESERVE(x) x
