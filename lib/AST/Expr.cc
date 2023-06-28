#include "Radium/AST/Expr.h"

#include "Radium/AST/ASTContext.h"
#include "Radium/AST/Type.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

namespace Radium {

using llvm::cast;

auto Expr::operator new(size_t bytes, ASTContext& context,
                        unsigned alignment) noexcept -> void* {
  return context.Allocate(bytes, alignment);
}

void Expr::Dump() const {
  Print(llvm::errs());
  llvm::errs() << '\n';
}

auto Expr::GetLocStart() const -> llvm::SMLoc {
  switch (kind_) {
    case ExprKind::IntegerLiteralKind:
      return cast<IntegerLiteral>(this)->loc_;
    case ExprKind::ParenExprKind:
      return cast<ParenExpr>(this)->l_paren_loc_;
    case ExprKind::BinaryAddExprKind:
    case ExprKind::BinarySubExprKind:
    case ExprKind::BinaryMulExprKind:
    case ExprKind::BinaryDivExprKind:
      return cast<BinaryExpr>(this)->GetLocStart();
  }
  llvm_unreachable("expression type not handled!");
}

void Expr::Print(llvm::raw_ostream& os, unsigned indent) const {
  switch (kind_) {
    case ExprKind::IntegerLiteralKind:
      cast<IntegerLiteral>(this)->Print(os, indent);
    case ExprKind::ParenExprKind:
      cast<ParenExpr>(this)->Print(os, indent);
    case ExprKind::BinaryAddExprKind:
    case ExprKind::BinarySubExprKind:
    case ExprKind::BinaryMulExprKind:
    case ExprKind::BinaryDivExprKind:
      cast<BinaryExpr>(this)->Print(os, indent);
  }
}

void IntegerLiteral::Print(llvm::raw_ostream& os, unsigned indent) const {
  os.indent(indent) << "(integer_literal type='";
  type_->Print(os);
  os << "' value=" << val_ << ")";
}

void ParenExpr::Print(llvm::raw_ostream& os, unsigned indent) const {
  os.indent(indent) << "(paren_expr type='";
  type_->Print(os);
  os << "'\n";
  sub_expr_->Print(os, indent + 1);
  os << ")";
}

void BinaryExpr::Print(llvm::raw_ostream& os, unsigned indent) const {
  os.indent(indent) << "(binary_expr type='";
  type_->Print(os);
  os << "'\n";
  lhs_->Print(os, indent + 1);
  os << "\n";
  rhs_->Print(os, indent + 1);
  os << ")";
}

}  // namespace Radium