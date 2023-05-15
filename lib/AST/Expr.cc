#include "radium/AST/Expr.h"

#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "radium/AST/ASTContext.h"
#include "radium/AST/Type.h"

using namespace Radium;
using llvm::cast;

void* Expr::operator new(size_t Bytes, ASTContext& C,
                         unsigned Alignment) noexcept {
  return C.Allocate(Bytes, Alignment);
}

void Expr::Dump() const {
  Print(llvm::errs());
  llvm::errs() << '\n';
}

llvm::SMLoc Expr::GetLocStart() const {
  switch (Kind) {
    case ExprKind::IntegerLiteralKind:
      return cast<IntegerLiteral>(this)->Loc;
    case ExprKind::ParenExprKind:
      return cast<ParenExpr>(this)->LParenLoc;
    case ExprKind::BinaryAddExprKind:
    case ExprKind::BinarySubExprKind:
    case ExprKind::BinaryMulExprKind:
    case ExprKind::BinaryDivExprKind:
      return cast<BinaryExpr>(this)->GetLocStart();
  }
  llvm_unreachable("expression type not handled!");
}

void Expr::Print(llvm::raw_ostream& OS, unsigned Indent) const {
  switch (Kind) {
    case ExprKind::IntegerLiteralKind:
      cast<IntegerLiteral>(this)->Print(OS, Indent);
    case ExprKind::ParenExprKind:
      cast<ParenExpr>(this)->Print(OS, Indent);
    case ExprKind::BinaryAddExprKind:
    case ExprKind::BinarySubExprKind:
    case ExprKind::BinaryMulExprKind:
    case ExprKind::BinaryDivExprKind:
      cast<BinaryExpr>(this)->Print(OS, Indent);
  }
}

void IntegerLiteral::Print(llvm::raw_ostream& OS, unsigned Indent) const {
  OS.indent(Indent) << "(integer_literal type='";
  Ty->Print(OS);
  OS << "' value=" << Val << ")";
}

void ParenExpr::Print(llvm::raw_ostream& OS, unsigned Indent) const {
  OS.indent(Indent) << "(paren_expr type='";
  Ty->Print(OS);
  OS << "'\n";
  SubExpr->Print(OS, Indent + 1);
  OS << ")";
}

void BinaryExpr::Print(llvm::raw_ostream& OS, unsigned Indent) const {
  OS.indent(Indent) << "(binary_expr type='";
  Ty->Print(OS);
  OS << "'\n";
  LHS->Print(OS, Indent + 1);
  OS << "\n";
  RHS->Print(OS, Indent + 1);
  OS << ")";
}