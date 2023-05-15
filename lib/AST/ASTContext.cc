#include "radium/AST/ASTContext.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Allocator.h"
#include "radium/AST/Decl.h"
#include "radium/AST/Type.h"

using namespace Radium;

using TupleTypesMapTy = llvm::FoldingSet<TupleType>;
using FunctionTypesMapTy =
    llvm::DenseMap<std::pair<Type*, Type*>, FunctionType*>;

ASTContext::ASTContext(llvm::SourceMgr& SrcMgr)
    : Allocator(new llvm::BumpPtrAllocator()),
      TupleTypes(new TupleTypesMapTy()),
      FunctionTypes(new FunctionTypesMapTy()),
      SrcMgr(SrcMgr),
      VoidType(GetTupleType(nullptr, 0)),  // void is aka "()"
      IntType(new(*this) BuiltinType(TypeKind::BuiltinIntKind)) {}

ASTContext::~ASTContext() {
  delete Allocator;
  delete static_cast<TupleTypesMapTy*>(TupleTypes);
  TupleTypes = nullptr;
  delete static_cast<FunctionTypesMapTy*>(FunctionTypes);
  FunctionTypes = nullptr;
}

void* ASTContext::Allocate(unsigned long Bytes, unsigned Alignment) {
  return Allocator->Allocate(Bytes, Alignment);
}

void TupleType::Profile(llvm::FoldingSetNodeID& ID, const TypeOrDecl* Fields,
                        unsigned NumFields) {
  //ID.AddInteger(NumFields);
  //for (unsigned i = 0; i != NumFields; ++i) {
  //  if (Type* Ty = Fields[i].dyn_cast<Type*>()) {
  //    ID.AddPointer(Ty);
  //  } else {
  //    ID.AddPointer(Fields[i].get<VarDecl*>());
  //  }
  //}
}

TupleType* ASTContext::GetTupleType(const TupleType::TypeOrDecl* Fields,
                                    unsigned NumFields) {
  llvm::FoldingSetNodeID ID;
  TupleType::Profile(ID, Fields, NumFields);

  TupleTypesMapTy& TupleTypesMap = *static_cast<TupleTypesMapTy*>(TupleTypes);

  void* InsertPos = nullptr;
  if (TupleType* TT = TupleTypesMap.FindNodeOrInsertPos(ID, InsertPos)) {
    return TT;
  }

  TupleType::TypeOrDecl* FieldsCopy =
      (TupleType::TypeOrDecl*)Allocate(sizeof(*Fields) * NumFields, 8);
  memcpy(FieldsCopy, Fields, sizeof(*Fields) * NumFields);

  TupleType* New = new (*this) TupleType(FieldsCopy, NumFields);
  TupleTypesMap.InsertNode(New, InsertPos);
  return New;
}

FunctionType* ASTContext::GetFunctionType(Type* Input, Type* Result) {
  FunctionType*& Entry =
      (*static_cast<FunctionTypesMapTy*>(FunctionTypes))[{Input, Result}];
  if (Entry)
    return Entry;

  Entry = new (*this) FunctionType(Input, Result);
  return Entry;
}