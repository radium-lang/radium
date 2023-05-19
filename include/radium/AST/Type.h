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
  friend class ASTContext;
  Type(const Type&);
  void operator=(const Type&);

 public:
  const TypeKind Kind;

  void Dump() const;

  void Print(llvm::raw_ostream& OS) const;

  static bool classof(const Type*) { return true; }

  void* operator new(size_t Bytes, ASTContext& C,
                     unsigned Alignment = 8) noexcept;

 protected:
  Type(TypeKind kind) : Kind(kind) {}

 public:
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

// class TupleTypeElt {
//  public:
//   Identifier Name;
//   Type* Ty;
// };

class TupleType : public Type, public llvm::FoldingSetNode {
 public:
  using TypeOrDecl = llvm::PointerUnion<Type*, VarDecl*>;
  const TypeOrDecl* const Fields;
  const unsigned NumFields;

  void Print(llvm::raw_ostream& OS) const;

  static bool classof(const TupleType*) { return true; }
  static bool classof(const Type* T) {
    return T->Kind == TypeKind::TupleTypeKind;
  }

  void Profile(llvm::FoldingSetNodeID& ID) { Profile(ID, Fields, NumFields); }
  static void Profile(llvm::FoldingSetNodeID& ID, const TypeOrDecl* Fields,
                      unsigned NumFields);

 private:
  TupleType(const TypeOrDecl* const Fields, unsigned NumFields)
      : Type(TypeKind::TupleTypeKind), Fields(Fields), NumFields(NumFields) {}
  friend class ASTContext;
};

class FunctionType : public Type {
 public:
  Type* const Input;
  Type* const Result;

  void Print(llvm::raw_ostream& OS) const;

  static bool classof(const FunctionType*) { return true; }
  static bool classof(const Type* T) {
    return T->Kind == TypeKind::FunctionTypeKind;
  }

 private:
  FunctionType(Type* Input, Type* Result)
      : Type(TypeKind::FunctionTypeKind), Input(Input), Result(Result) {}
  friend class ASTContext;
};

}  // namespace Radium

namespace llvm {
static inline llvm::raw_ostream& operator<<(llvm::raw_ostream& OS,
                                            const Radium::Type& T) {
  T.Print(OS);
  return OS;
}
}  // namespace llvm

#endif  // RADIUM_AST_TYPE_H