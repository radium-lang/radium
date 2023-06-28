#ifndef RADIUM_AST_EXPR_H
#define RADIUM_AST_EXPR_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace llvm {
class raw_ostream;
}  // namespace llvm

namespace Radium {

class ASTContext;
class Type;

enum ExprKind : uint8_t {
  IntegerLiteralKind,
  ParenExprKind,

  BinaryAddExprKind,
  BinarySubExprKind,
  BinaryMulExprKind,
  BinaryDivExprKind,

  First_BinaryExpr = BinaryAddExprKind,
  Last_BinaryExpr = BinaryDivExprKind,
};

class Expr {
 public:
  Expr(ExprKind kind, Type* type) : kind_(kind), type_(type) {}

  Expr(const Expr&) = delete;
  void operator=(const Expr&) = delete;

  auto GetLocStart() const -> llvm::SMLoc;

  void Dump() const;

  void Print(llvm::raw_ostream& os, unsigned indent = 0) const;

  static auto classof(const Expr* /*unused*/) -> bool { return true; }

  auto operator new(size_t bytes) noexcept -> void*;
  void operator delete(void* data) noexcept;
  auto operator new(size_t bytes, void* mem) noexcept -> void*;
  auto operator new(size_t bytes, ASTContext& context,
                    unsigned alignment = 8) noexcept -> void*;

  const ExprKind kind_;
  Type* type_;
};

class IntegerLiteral : public Expr {
 public:
  IntegerLiteral(llvm::StringRef val, llvm::SMLoc loc, Type* type)
      : Expr(ExprKind::IntegerLiteralKind, type), val_(val), loc_(loc) {}

  void Print(llvm::raw_ostream& os, unsigned indent = 0) const;

  static auto classof(const IntegerLiteral* /*unused*/) -> bool { return true; }
  static auto classof(const Expr* expr) -> bool {
    return expr->kind_ == ExprKind::IntegerLiteralKind;
  }

  llvm::StringRef val_;
  llvm::SMLoc loc_;
};

class ParenExpr : public Expr {
 public:
  ParenExpr(llvm::SMLoc l_paren_loc, Expr* sub_expr, llvm::SMLoc r_paren_loc,
            Type* type)
      : Expr(ExprKind::ParenExprKind, type),
        l_paren_loc_(l_paren_loc),
        sub_expr_(sub_expr),
        r_paren_loc_(r_paren_loc) {}

  void Print(llvm::raw_ostream& os, unsigned indent = 0) const;

  static auto classof(const ParenExpr* /*unused*/) -> bool { return true; }
  static auto classof(const Expr* expr) -> bool {
    return expr->kind_ == ExprKind::ParenExprKind;
  }

  llvm::SMLoc l_paren_loc_;
  Expr* sub_expr_;
  llvm::SMLoc r_paren_loc_;
};

class BinaryExpr : public Expr {
 public:
  BinaryExpr(ExprKind kind, Expr* lhs, llvm::SMLoc op_loc, Expr* rhs,
             Type* type)
      : Expr(kind, type), lhs_(lhs), op_loc_(op_loc), rhs_(rhs) {}

  void Print(llvm::raw_ostream& os, unsigned indent = 0) const;

  static auto classof(const BinaryExpr* /*unused*/) -> bool { return true; }
  static auto classof(const Expr* expr) -> bool {
    return expr->kind_ >= ExprKind::First_BinaryExpr &&
           expr->kind_ <= ExprKind::Last_BinaryExpr;
  }

  Expr* lhs_;
  Expr* rhs_;
  llvm::SMLoc op_loc_;
};

}  // namespace Radium

#endif  // RADIUM_AST_EXPR_H