#ifndef RADIUM_AST_DECL_H
#define RADIUM_AST_DECL_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace llvm {
class StringRef;
class raw_ostream;
}  // namespace llvm

namespace Radium {

class ASTContext;
class Type;
class Expr;

enum DeclKind : uint8_t {
  VarDeclKind,
  // FuncDeclKind,
};

class Decl {
 protected:
  Decl(DeclKind kind) : kind_(kind) {}

 public:
  Decl(const Decl&) = delete;
  void operator=(const Decl&) = delete;

  auto GetKind() const -> DeclKind { return kind_; }

  void Dump() const;

  void Print(llvm::raw_ostream& os, unsigned indent = 0) const;

  static auto classof(const Decl* /*unused*/) -> bool { return true; }

  auto operator new(size_t bytes) noexcept -> void*;
  void operator delete(void* data) noexcept;
  auto operator new(size_t bytes, void* mem) noexcept -> void*;
  auto operator new(size_t bytes, ASTContext& c,
                    unsigned alignment = 8) noexcept -> void*;

  const DeclKind kind_;
};

class VarDecl : public Decl {
 public:
  VarDecl(llvm::SMLoc var_loc, llvm::StringRef name, Type* type, Expr* init)
      : Decl(DeclKind::VarDeclKind),
        var_loc_(var_loc),
        name_(name),
        type_(type),
        init_(init) {}

  void Print(llvm::raw_ostream& os, unsigned indent = 0) const;

  static auto classof(const VarDecl*  /*D*/) -> bool { return true; }
  static auto classof(const Decl* decl) -> bool {
    return decl->GetKind() == DeclKind::VarDeclKind;
  }

  friend class ASTContext;

  llvm::SMLoc var_loc_;
  llvm::StringRef name_;
  Type* type_;
  Expr* init_;
};

}  // namespace Radium

#endif  // RADIUM_AST_DECL_H