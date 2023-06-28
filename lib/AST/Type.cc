#include "Radium/AST/Type.h"

#include "Radium/AST/ASTContext.h"
#include "Radium/AST/Decl.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

namespace Radium {

using llvm::cast;

auto Type::operator new(size_t bytes, ASTContext& context,
                        unsigned alignment) noexcept -> void* {
  return context.Allocate(bytes, alignment);
}

void Type::Dump() const { llvm::errs() << *this << '\n'; }

void Type::Print(llvm::raw_ostream& os) const {
  switch (kind_) {
    case TypeKind::BuiltinIntKind:
      cast<BuiltinType>(this)->Print(os);
    case TypeKind::TupleTypeKind:
      cast<TupleType>(this)->Print(os);
    case TypeKind::FunctionTypeKind:
      cast<FunctionType>(this)->Print(os);
    default:
      llvm_unreachable("Unknown type kind");
  }
}

void BuiltinType::Print(llvm::raw_ostream& os) const {
  assert(kind_ == TypeKind::BuiltinIntKind && "Only one builtin type!");
  os << "int";
}

void TupleType::Print(llvm::raw_ostream& os) const {
  os << "(";

  for (unsigned i = 0, e = num_fields_; i != e; ++i) {
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
  os << ")";
}

void FunctionType::Print(llvm::raw_ostream& os) const {
  os << *input_ << " -> " << *result_;
}

}  // namespace Radium