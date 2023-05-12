#ifndef RADIUM_PARSE_PARSER_H
#define RADIUM_PARSE_PARSER_H

#include <llvm/Support/SMLoc.h>

#include "radium/Parse/Token.h"

namespace llvm {
class SourceMgr;
}  // namespace llvm

namespace Radium {

class Lexer;
class Sema;

class Parser {
  llvm::SourceMgr& SrcMgr;
  Lexer* L;
  Sema* S;

  Token T;

  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;

 public:
  Parser(unsigned BufferID, llvm::SourceMgr& SrcMgr);
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

  void SkipUtil(Tok::TokenKind K);

  void Note(llvm::SMLoc Loc, const char* Message);
  void Warning(llvm::SMLoc Loc, const char* Message);
  void Error(llvm::SMLoc Loc, const char* Message);

  bool ParseIdentifier(llvm::StringRef& Result, const char* Message = 0,
                       Tok::TokenKind SkipToTok = Tok::TokenKind::Unknown);

  bool ParseToken(Tok::TokenKind K, const char* Message,
                  Tok::TokenKind SkipToTok = Tok::TokenKind::Unknown);

  // Decl
  void ParseDeclTopLevel();
  void ParseDeclVar();

  // Type
  bool ParseType(const char* Message = 0);

  // Expr
  bool ParseExpr(const char* Message = 0);
  bool ParseExprPrimary(const char* Message = 0);
  bool ParseExprBinaryRHS(unsigned MinPrecedence = 1);
};

}  // namespace Radium

#endif  // RADIUM_PARSE_PARSER_H