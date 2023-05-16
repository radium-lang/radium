#ifndef RADIUM_PARSE_PARSER_H
#define RADIUM_PARSE_PARSER_H

#include "radium/Parse/Token.h"

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
  ASTConsumer& Consumer;
  llvm::SourceMgr& SrcMgr;
  Lexer& L;
  Sema& S;

  Token T;

  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;

 public:
  Parser(unsigned BufferID, ASTConsumer& Consumer);
  ~Parser();

  void ParseTranslationUnit();

 private:
  void ConsumeToken();
  void ConsumeToken(Tok::TokenKind K) {
    assert(T.Is(K) && "Consuming wrong token kind");
    ConsumeToken();
  }

  bool ConsumeIf(Tok::TokenKind K) {
    if (T.IsNot(K))
      return false;
    ConsumeToken(K);
    return true;
  }

  void SkipUntil(Tok::TokenKind K);

  void Note(llvm::SMLoc Loc, const char* Message);
  void Warning(llvm::SMLoc Loc, const char* Message);
  void Error(llvm::SMLoc Loc, const char* Message);

  bool ParseIdentifier(llvm::StringRef& Result, const char* Message = 0,
                       Tok::TokenKind SkipToTok = Tok::TokenKind::Unknown);

  bool ParseToken(Tok::TokenKind K, const char* Message,
                  Tok::TokenKind SkipToTok = Tok::TokenKind::Unknown);

  // Decl
  void ParseDeclTopLevel(Decl*& Result);
  void ParseDeclVar(Decl*& Result);

  // Type
  bool ParseType(Type*& Result, const char* Message = 0);
  bool ParseTypeOrDeclVar(llvm::PointerUnion<Type*, Decl*>& Result,
                          const char* Message = 0);
  bool ParseTypeTuple(Type*& Result);

  // Expr
  bool ParseExpr(Expr*& Result, const char* Message = 0);
  bool ParseExprPrimary(Expr*& Result, const char* Message = 0);
  bool ParseExprBinaryRHS(Expr*& Result, unsigned MinPrecedence = 1);
};

}  // namespace Radium

#endif  // RADIUM_PARSE_PARSER_H