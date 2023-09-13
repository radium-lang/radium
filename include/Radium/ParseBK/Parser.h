#ifndef RADIUM_PARSE_PARSER_H
#define RADIUM_PARSE_PARSER_H

#include "Radium/Parse/Token.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
class SourceMgr;
}  // namespace llvm

namespace Radium {

class Lexer;
class Sema;
class Expr;
class Type;
class Decl;
class ASTContext;
class ASTConsumer;

class Parser {
 public:
  Parser(unsigned buffer_id, ASTConsumer& consumer);
  ~Parser();

  void ParseTranslationUnit();

 private:
  void ConsumeToken();
  void ConsumeToken(Tok::TokenKind kind) {
    assert(token_.Is(kind) && "Consuming wrong token kind");
    ConsumeToken();
  }

  auto ConsumeIf(Tok::TokenKind kind) -> bool {
    if (token_.IsNot(kind)) {
      return false;
    }
    ConsumeToken(kind);
    return true;
  }

  void SkipUntil(Tok::TokenKind kind);

  void Note(llvm::SMLoc loc, const char* message);
  void Warning(llvm::SMLoc loc, const char* message);
  void Error(llvm::SMLoc loc, const char* message);

  auto ParseIdentifier(llvm::StringRef& result, const char* message = nullptr,
                       Tok::TokenKind skip_to_tok = Tok::TokenKind::Unknown) -> bool;

  auto ParseToken(Tok::TokenKind kind, const char* message,
                  Tok::TokenKind skip_to_tok = Tok::TokenKind::Unknown) -> bool;

  // Decl
  void ParseDeclTopLevel(Decl*& result);
  void ParseDeclVar(Decl*& result);

  // Type
  auto ParseType(Type*& result, const char* message = nullptr) -> bool;
  auto ParseTypeOrDeclVar(llvm::PointerUnion<Type*, Decl*>& result,
                          const char* message = nullptr) -> bool;
  auto ParseTypeTuple(Type*& result) -> bool;

  // Expr
  auto ParseExpr(Expr*& result, const char* message = nullptr) -> bool;
  auto ParseExprPrimary(Expr*& result, const char* message = nullptr) -> bool;
  auto ParseExprBinaryRHS(Expr*& result, unsigned min_precedence = 1) -> bool;

  Parser(const Parser&);
  void operator=(const Parser&);

  ASTConsumer& consumer_;
  llvm::SourceMgr& src_mgr_;
  Lexer& lexer_;
  Sema& sema_;
  Token token_;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_PARSER_H