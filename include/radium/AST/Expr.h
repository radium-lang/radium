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
  Expr(const Expr&);
  void operator=(const Expr&);

 public:
  Expr(ExprKind Kind, Type* Ty) : Kind(Kind), Ty(Ty) {}

  llvm::SMLoc GetLocStart() const;

  void Dump() const;

  void Print(llvm::raw_ostream& OS, unsigned Indent = 0) const;

  static bool classof(const Expr*) { return true; }

 public:
  void* operator new(size_t Bytes) noexcept;
  void operator delete(void* Data) noexcept;
  void* operator new(size_t Bytes, void* Mem) noexcept;

 public:
  void* operator new(size_t Bytes, ASTContext& C,
                     unsigned Alignment = 8) noexcept;

 public:
  const ExprKind Kind;
  Type* Ty;
};

class IntegerLiteral : public Expr {
 public:
  llvm::StringRef Val;
  llvm::SMLoc Loc;

  IntegerLiteral(llvm::StringRef V, llvm::SMLoc L, Type* Ty)
      : Expr(ExprKind::IntegerLiteralKind, Ty), Val(V), Loc(L) {}

  void Print(llvm::raw_ostream& OS, unsigned Indent = 0) const;

  static bool classof(const IntegerLiteral*) { return true; }
  static bool classof(const Expr* E) {
    return E->Kind == ExprKind::IntegerLiteralKind;
  }
};

class ParenExpr : public Expr {
 public:
  llvm::SMLoc LParenLoc;
  Expr* SubExpr;
  llvm::SMLoc RParenLoc;

  ParenExpr(llvm::SMLoc LParenLoc, Expr* SubExpr, llvm::SMLoc RParenLoc,
            Type* Ty)
      : Expr(ExprKind::ParenExprKind, Ty),
        LParenLoc(LParenLoc),
        SubExpr(SubExpr),
        RParenLoc(RParenLoc) {}

  void Print(llvm::raw_ostream& OS, unsigned Indent = 0) const;

  static bool classof(const ParenExpr*) { return true; }
  static bool classof(const Expr* E) {
    return E->Kind == ExprKind::ParenExprKind;
  }
};

class BinaryExpr : public Expr {
 public:
  Expr* LHS;
  Expr* RHS;
  llvm::SMLoc OpLoc;

  BinaryExpr(ExprKind Kind, Expr* LHS, llvm::SMLoc OpLoc, Expr* RHS, Type* Ty)
      : Expr(Kind, Ty), LHS(LHS), OpLoc(OpLoc), RHS(RHS) {}

  void Print(llvm::raw_ostream& OS, unsigned Indent = 0) const;

  static bool classof(const BinaryExpr*) { return true; }
  static bool classof(const Expr* E) {
    return E->Kind >= ExprKind::First_BinaryExpr &&
           E->Kind <= ExprKind::Last_BinaryExpr;
  }
};

}  // namespace Radium

#endif  // RADIUM_AST_EXPR_H