#include "radium/AST/Decl.h"

#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "radium/AST/ASTContext.h"
#include "radium/AST/Expr.h"
#include "radium/AST/Type.h"

using namespace Radium;
using llvm::cast;

void* Decl::operator new(size_t Bytes, ASTContext& C,
                         unsigned Alignment) noexcept {
  return C.Allocate(Bytes, Alignment);
}

void Decl::Dump() const {
  Print(llvm::errs());
  llvm::errs() << '\n';
}

void Decl::Print(llvm::raw_ostream& OS, unsigned Indent) const {
  switch (Kind) {
    case DeclKind::VarDeclKind:
      cast<VarDecl>(this)->Print(OS, Indent);
      break;
    default:
      llvm_unreachable("Unknown decl kind");
  }
}

void VarDecl::Print(llvm::raw_ostream& OS, unsigned Indent) const {
  OS.indent(Indent) << "(vardecl '" << Name << "' type='";
  Ty->Print(OS);
  OS << "'";

  if (Init) {
    OS << "\n";
    Init->Print(OS, Indent + 1);
  }
  OS << ")";
}