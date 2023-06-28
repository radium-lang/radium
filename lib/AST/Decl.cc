#include "Radium/AST/Decl.h"

#include "Radium/AST/ASTContext.h"
#include "Radium/AST/Expr.h"
#include "Radium/AST/Type.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

namespace Radium {

using llvm::cast;

auto Decl::operator new(size_t bytes, ASTContext& context,
                        unsigned alignment) noexcept -> void* {
  return context.Allocate(bytes, alignment);
}

void Decl::Dump() const {
  Print(llvm::errs());
  llvm::errs() << '\n';
}

void Decl::Print(llvm::raw_ostream& os, unsigned indent) const {
  switch (kind_) {
    case DeclKind::VarDeclKind:
      cast<VarDecl>(this)->Print(os, indent);
      break;
    default:
      llvm_unreachable("Unknown decl kind");
  }
}

void VarDecl::Print(llvm::raw_ostream& os, unsigned indent) const {
  os.indent(indent) << "(vardecl '" << name_ << "' type='";
  type_->Print(os);
  os << "'";

  if (init_) {
    os << "\n";
    init_->Print(os, indent + 1);
  }
  os << ")";
}

}  // namespace Radium