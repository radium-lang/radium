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
  Decl(const Decl&);
  void operator=(const Decl&);

 protected:
  Decl(DeclKind Kind) : Kind(Kind) {}

 public:
  DeclKind GetKind() const { return Kind; }

  void Dump() const;

  void Print(llvm::raw_ostream& OS, unsigned Indent = 0) const;

  static bool classof(const Decl*) { return true; }

 public:
  void* operator new(size_t Bytes) noexcept;
  void operator delete(void* Data) noexcept;
  void* operator new(size_t Bytes, void* Mem) noexcept;

 public:
  void* operator new(size_t Bytes, ASTContext& C,
                     unsigned Alignment = 8) noexcept;

 public:
  const DeclKind Kind;
};

class VarDecl : public Decl {
  friend class ASTContext;

 public:
  llvm::SMLoc VarLoc;
  llvm::StringRef Name;
  Type* Ty;
  Expr* Init;

  VarDecl(llvm::SMLoc VarLoc, llvm::StringRef Name, Type* Ty, Expr* Init)
      : Decl(DeclKind::VarDeclKind),
        VarLoc(VarLoc),
        Name(Name),
        Ty(Ty),
        Init(Init) {}

  void Print(llvm::raw_ostream& OS, unsigned Indent = 0) const;

  static bool classof(const VarDecl* D) { return true; }
  static bool classof(const Decl* D) {
    return D->GetKind() == DeclKind::VarDeclKind;
  }
};

}  // namespace Radium

#endif  // RADIUM_AST_DECL_H