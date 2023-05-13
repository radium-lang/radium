#ifndef RADIUM_AST_TYPE_H
#define RADIUM_AST_TYPE_H

#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
class raw_ostream;
}  // namespace llvm

namespace Radium {

class ASTContext;
class VarDecl;

enum class TypeKind : uint8_t {
  BuiltinIntKind,
  TupleTypeKind,
  FunctionTypeKind,

  Builtin_First = BuiltinIntKind,
  Builtin_Last = BuiltinIntKind,
};

class Type {
  Type(const Type&) = delete;
  void operator=(const Type&) = delete;

 public:
  const TypeKind Kind;

  void Dump() const;

  void Print(llvm::raw_ostream& OS) const;

  static bool classof(const Type*) { return true; }

  void* operator new(size_t Bytes, ASTContext& C,
                     unsigned Alignment = 8) noexcept;

 protected:
  Type(TypeKind kind) : Kind(kind) {}

 private:
  void* operator new(size_t Bytes) noexcept;
  void operator delete(void* Data) noexcept;
  void* operator new(size_t Bytes, void* Mem) noexcept;
};

class BuiltinType : public Type {
  friend class ASTContext;
  BuiltinType(TypeKind K) : Type(K) {}

 public:
  void Print(llvm::raw_ostream& OS) const;

  static bool classof(const BuiltinType*) { return true; }
  static bool classof(const Type* T) {
    return T->Kind >= TypeKind::Builtin_First &&
           T->Kind <= TypeKind::Builtin_Last;
  }
};

class TupleType : public Type, public llvm::FoldingSetNode {

};

}  // namespace Radium

#endif  // RADIUM_AST_TYPE_H