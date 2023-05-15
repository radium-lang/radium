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
  ASTContext(const ASTContext&) = delete;
  void operator=(const ASTContext&) = delete;

 public:
  ASTContext(llvm::SourceMgr& SrcMgr);
  ~ASTContext();

  TupleType* GetTupleType(const llvm::PointerUnion<Type*, VarDecl*>* Fields,
                          unsigned NumFields);

  FunctionType* GetFunctionType(Type* Input, Type* Result);

  void* Allocate(unsigned long Bytes, unsigned Alignment);

 public:
  llvm::SourceMgr& SrcMgr;
  Type* const VoidType;
  Type* const IntType;

 private:
  llvm::BumpPtrAllocator* Allocator;
  void* TupleTypes;
  void* FunctionTypes;
};

}  // namespace Radium

#endif  // RADIUM_AST_ASTCONTEXT_H