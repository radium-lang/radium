#include "radium/AST/Type.h"

#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "radium/AST/ASTContext.h"
#include "radium/AST/Decl.h"

using namespace Radium;
using llvm::cast;

void* Type::operator new(size_t Bytes, ASTContext& C,
                         unsigned Alignment) noexcept {
  return C.Allocate(Bytes, Alignment);
}

void Type::Dump() const { llvm::errs() << *this << '\n'; }

void Type::Print(llvm::raw_ostream& OS) const {
  switch (Kind) {
    case TypeKind::BuiltinIntKind:
      cast<BuiltinType>(this)->Print(OS);
    case TypeKind::TupleTypeKind:
      cast<TupleType>(this)->Print(OS);
    case TypeKind::FunctionTypeKind:
      cast<FunctionType>(this)->Print(OS);
    default:
      llvm_unreachable("Unknown type kind");
  }
}

void BuiltinType::Print(llvm::raw_ostream& OS) const {
  assert(Kind == TypeKind::BuiltinIntKind && "Only one builtin type!");
  OS << "int";
}

void TupleType::Print(llvm::raw_ostream& OS) const {
  OS << "(";

  for (unsigned i = 0, e = NumFields; i != e; ++i) {
  //  if (i)
  //    OS << ", ";
  //  const TypeOrDecl& Field = Fields[i];
//
  //  if (Type* Ty = Field.dyn_cast<Type*>()) {
  //    OS << *Ty;
  //    continue;
  //  }
//
  //  VarDecl* VD = Field.get<VarDecl*>();
  //  OS << "var " << VD->Name << ": ";
  //  VD->Ty->Print(OS);
  //  assert(VD->Init == 0 && "Don't handle inits");
  }
  OS << ")";
}

void FunctionType::Print(llvm::raw_ostream& OS) const {
  OS << *Input << " -> " << *Result;
}