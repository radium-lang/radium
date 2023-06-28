#include "Radium/AST/ASTContext.h"

#include "Radium/AST/Decl.h"
#include "Radium/AST/Type.h"
#include "llvm/ADT/DenseMap.h"

namespace Radium {

using TupleTypesMapTy = llvm::FoldingSet<TupleType>;
using FunctionTypesMapTy =
    llvm::DenseMap<std::pair<Type*, Type*>, FunctionType*>;

ASTContext::ASTContext(llvm::SourceMgr& src_mgr)
    : allocator_(new llvm::BumpPtrAllocator()),
      tuple_types_(new TupleTypesMapTy()),
      function_types_(new FunctionTypesMapTy()),
      src_mgr_(src_mgr),
      void_type_(GetTupleType(nullptr, 0)),  // void is aka "()"
      int_type_(new(*this) BuiltinType(TypeKind::BuiltinIntKind)) {}

ASTContext::~ASTContext() {
  delete allocator_;
  delete static_cast<TupleTypesMapTy*>(tuple_types_);
  tuple_types_ = nullptr;
  delete static_cast<FunctionTypesMapTy*>(function_types_);
  function_types_ = nullptr;
}

auto ASTContext::Allocate(uint64_t bytes, unsigned alignment) -> void* {
  return allocator_->Allocate(bytes, alignment);
}

void TupleType::Profile(llvm::FoldingSetNodeID& id, const TypeOrDecl* fields,
                        unsigned num_fields) {
  // ID.AddInteger(NumFields);
  // for (unsigned i = 0; i != NumFields; ++i) {
  //   if (Type* Ty = Fields[i].dyn_cast<Type*>()) {
  //     ID.AddPointer(Ty);
  //   } else {
  //     ID.AddPointer(Fields[i].get<VarDecl*>());
  //   }
  // }
}

auto ASTContext::GetTupleType(const TupleType::TypeOrDecl* fields,
                              unsigned num_fields) -> TupleType* {
  llvm::FoldingSetNodeID id;
  TupleType::Profile(id, fields, num_fields);

  TupleTypesMapTy& tuple_types_map =
      *static_cast<TupleTypesMapTy*>(tuple_types_);

  void* insert_pos = nullptr;
  if (TupleType* tt = tuple_types_map.FindNodeOrInsertPos(id, insert_pos)) {
    return tt;
  }

  auto* fields_copy = static_cast<TupleType::TypeOrDecl*>(
      Allocate(sizeof(*fields) * num_fields, 8));
  memcpy(fields_copy, fields, sizeof(*fields) * num_fields);

  auto* new_tuple = new (*this) TupleType(fields_copy, num_fields);
  tuple_types_map.InsertNode(new_tuple, insert_pos);
  return new_tuple;
}

auto ASTContext::GetFunctionType(Type* input, Type* result) -> FunctionType* {
  FunctionType*& entry =
      (*static_cast<FunctionTypesMapTy*>(function_types_))[{input, result}];
  if (entry) {
    return entry;
  }

  entry = new (*this) FunctionType(input, result);
  return entry;
}

}  // namespace Radium