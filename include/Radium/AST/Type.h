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
 public:
  Type(const Type&) = delete;
  void operator=(const Type&) = delete;

  void Dump() const;

  void Print(llvm::raw_ostream& os) const;

  static auto classof(const Type* /*unused*/) -> bool { return true; }

  auto operator new(size_t bytes, ASTContext& context,
                    unsigned alignment = 8) noexcept -> void*;

 protected:
  Type(TypeKind kind) : kind_(kind) {}

 public:
  friend class ASTContext;

  auto operator new(size_t bytes) noexcept -> void*;
  auto operator delete(void* data) noexcept -> void;
  auto operator new(size_t bytes, void* mem) noexcept -> void*;

  const TypeKind kind_;
};

class BuiltinType : public Type {
 public:
  void Print(llvm::raw_ostream& os) const;

  static auto classof(const BuiltinType* /*unused*/) -> bool { return true; }
  static auto classof(const Type* t) -> bool {
    return t->kind_ >= TypeKind::Builtin_First &&
           t->kind_ <= TypeKind::Builtin_Last;
  }

 private:
  friend class ASTContext;
  BuiltinType(TypeKind k) : Type(k) {}
};

// class TupleTypeElt {
//  public:
//   Identifier Name;
//   Type* Ty;
// };

class TupleType : public Type, public llvm::FoldingSetNode {
 public:
  using TypeOrDecl = llvm::PointerUnion<Type*, VarDecl*>;

  void Print(llvm::raw_ostream& os) const;

  static auto classof(const TupleType* /*unused*/) -> bool { return true; }
  static auto classof(const Type* t) -> bool {
    return t->kind_ == TypeKind::TupleTypeKind;
  }

  void Profile(llvm::FoldingSetNodeID& id) {
    Profile(id, fields_, num_fields_);
  }
  static void Profile(llvm::FoldingSetNodeID& id, const TypeOrDecl* fields,
                      unsigned num_fields);

 private:
  TupleType(const TypeOrDecl* const fields, unsigned num_fields)
      : Type(TypeKind::TupleTypeKind),
        fields_(fields),
        num_fields_(num_fields) {}
  friend class ASTContext;

 public:
  const TypeOrDecl* const fields_;
  const unsigned num_fields_;
};

class FunctionType : public Type {
 public:
  void Print(llvm::raw_ostream& os) const;

  static auto classof(const FunctionType* /*unused*/) -> bool { return true; }
  static auto classof(const Type* t) -> bool {
    return t->kind_ == TypeKind::FunctionTypeKind;
  }

 private:
  FunctionType(Type* input, Type* result)
      : Type(TypeKind::FunctionTypeKind), input_(input), result_(result) {}
  friend class ASTContext;

 public:
  Type* const input_;
  Type* const result_;
};

}  // namespace Radium

namespace llvm {
static inline auto operator<<(llvm::raw_ostream& os, const Radium::Type& t)
    -> llvm::raw_ostream& {
  t.Print(os);
  return os;
}
}  // namespace llvm

#endif  // RADIUM_AST_TYPE_H