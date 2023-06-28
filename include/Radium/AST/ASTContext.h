#ifndef RADIUM_AST_ASTCONTEXT_H
#define RADIUM_AST_ASTCONTEXT_H

#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Allocator.h"

namespace llvm {
class SourceMgr;
template <typename T>
class FoldingSet;
}  // namespace llvm

namespace Radium {

class Type;
class TupleType;
class FunctionType;
class VarDecl;

class ASTContext {
  ASTContext(const ASTContext&);
  void operator=(const ASTContext&);

 public:
  ASTContext(llvm::SourceMgr& src_mgr);
  ~ASTContext();

  auto GetTupleType(const llvm::PointerUnion<Type*, VarDecl*>* fields,
                    unsigned num_fields) -> TupleType*;

  auto GetFunctionType(Type* input, Type* result) -> FunctionType*;

  auto Allocate(uint64_t bytes, unsigned alignment) -> void*;

  llvm::SourceMgr& src_mgr_;
  Type* const void_type_;
  Type* const int_type_;

 private:
  llvm::BumpPtrAllocator* allocator_;
  void* tuple_types_;
  void* function_types_;
};

}  // namespace Radium

#endif  // RADIUM_AST_ASTCONTEXT_H