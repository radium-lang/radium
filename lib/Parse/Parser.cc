#include "radium/Parse/Parser.h"

#include "llvm/Support/SourceMgr.h"
#include "radium/Parse/Lexer.h"
#include "radium/Sema/Sema.h"

using namespace Radium;

Parser::Parser(unsigned BufferID, llvm::SourceMgr& SM) : SrcMgr(SM) {
  L = new Lexer(BufferID, SM);
  S = new Sema();
}

Parser::~Parser() {
  delete S;
  delete L;
}

void Parser::Note(llvm::SMLoc Loc, const char* Message) {
  SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Note, Message);
}

void Parser::Warning(llvm::SMLoc Loc, const char* Message) {
  SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Warning, Message);
}

void Parser::Error(llvm::SMLoc Loc, const char* Message) {
  SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Error, Message);
}

void Parser::ConsumeToken() {
  assert(T.IsNot(Tok::TokenKind::Eof) && "Consuming past eof!");
  L->Lex(T);
}

void Parser::SkipUtil(Tok::TokenKind K) {
  if (K == Tok::TokenKind::Unknown)
    return;

  while (T.IsNot(K) && T.IsNot(Tok::TokenKind::Eof)) {
    switch (T.GetKind()) {
      default:
        ConsumeToken();
        break;
    }
  }
}

bool Parser::ParseIdentifier(llvm::StringRef& Result, const char* Message,
                             Tok::TokenKind SkipToTok) {
  if (T.Is(Tok::TokenKind::Identifier)) {
    Result = T.GetText();
    ConsumeToken();
    return false;
  }

  Error(T.GetLocation(), Message ? Message : "Expected identifier");
  return true;
}

bool Parser::ParseToken(Tok::TokenKind K, const char* Message,
                        Tok::TokenKind SkipToTok) {
  if (T.Is(K)) {
    ConsumeToken();
    return false;
  }

  Error(T.GetLocation(), Message ? Message : "Expected token");
  SkipUtil(SkipToTok);
  return true;
}

/// translation-unit ::= decl-top-level*
void Parser::ParseTranslationUnit() {
  ConsumeToken();
  while (T.IsNot(Tok::TokenKind::Eof)) {
    ParseDeclTopLevel();
  }
}

/// decl-top-level ::= decl-var
///                 |  ';'
void Parser::ParseDeclTopLevel() {
  switch (T.GetKind()) {
    default:
      Error(T.GetLocation(), "Expected a top level declaration");
      return SkipUtil(Tok::TokenKind::Semi);
    case Tok::TokenKind::KW_var:
      return ParseDeclVar();
    case Tok::TokenKind::Semi:
      ConsumeToken(Tok::TokenKind::Semi);
  }
}

/// decl-var ::= 'var' identifier ':' type ';'
///           |  'var' identifier ':' type '=' expr ';'
///           |  'var' identifier ':' '=' expr ';'
void Parser::ParseDeclVar() {
  ConsumeToken(Tok::TokenKind::KW_var);

  llvm::StringRef Identifier;
  if (ParseIdentifier(Identifier, "Expected identifier in var declaration"))
    return SkipUtil(Tok::TokenKind::Semi);

  if (ConsumeIf(Tok::TokenKind::Colon) &&
      ParseType("Expected type in var declaration")) {
    return SkipUtil(Tok::TokenKind::Semi);
  }

  if (ConsumeIf(Tok::TokenKind::Equal) &&
      ParseExpr("Expected expression in var declaration")) {
    return SkipUtil(Tok::TokenKind::Semi);
  }
  ParseToken(Tok::TokenKind::Semi, "Expected ';' at end of var declaration",
             Tok::TokenKind::Semi);
}

/// type ::= 'int'
bool Parser::ParseType(const char* Message) {
  switch (T.GetKind()) {
    case Tok::TokenKind::KW_int:
      ConsumeToken(Tok::TokenKind::KW_int);
      return false;
    default:
      Error(T.GetLocation(), Message ? Message : "Expected type");
      return true;
  }
}

/// expr ::= expr-primary
///       |  expr-binary-rhs
///       |  expr-primary expr-binary-rhs
bool Parser::ParseExpr(const char* Message) {
  return ParseExprPrimary(Message) || ParseExprBinaryRHS();
}

/// expr-primary ::= Numeric_Constant
///               |  '(' expr ')'
bool Parser::ParseExprPrimary(const char* Message) {
  switch (T.GetKind()) {
    case Tok::TokenKind::Numeric_Constant:
      ConsumeToken(Tok::TokenKind::Numeric_Constant);
      return false;
    case Tok::TokenKind::LParen: {
      llvm::SMLoc LPLoc = T.GetLocation();
      ConsumeToken(Tok::TokenKind::LParen);
      if (ParseExpr("Expected expression in parentheses"))
        return true;
      if (ParseToken(Tok::TokenKind::RParen,
                     "Expected ')' at end of expression")) {
        Note(LPLoc, "to match this opening '('");
        return true;
      }
      return false;
    }
    default:
      Error(T.GetLocation(), Message ? Message : "Expected expression");
      return true;
  }
}

namespace Prec {

enum Level {
  Unknown = 0,     // Not a binary operator.
  Additive,        // '+', '-'
  Multiplicative,  // '*', '/', '%'
};

}  // namespace Prec

static Prec::Level GetBinOpPrecedence(Tok::TokenKind kind) {
  switch (kind) {
    default:
      return Prec::Unknown;
    case Tok::TokenKind::Plus:
    case Tok::TokenKind::Minus:
      return Prec::Additive;
    case Tok::TokenKind::Star:
    case Tok::TokenKind::Slash:
      // case Tok::TokenKind::Percent:
      return Prec::Multiplicative;
  }
}

/// expr-binary-rhs ::= (binary-op expr-primary)*
bool Parser::ParseExprBinaryRHS(unsigned MinPrec) {
  Prec::Level NextTokPrec = GetBinOpPrecedence(T.GetKind());
  while (true) {
    if (NextTokPrec < (Prec::Level)MinPrec)
      return false;

    Token OpToken = T;
    ConsumeToken();

    // TODO: Support ternary operators

    if (ParseExprPrimary("Expected expression after binary operator"))
      return true;

    Prec::Level ThisPrec = NextTokPrec;
    NextTokPrec = GetBinOpPrecedence(T.GetKind());

    // TODO: All operator are left associative

    if (ThisPrec < NextTokPrec) {
      if (ParseExprBinaryRHS(ThisPrec + 1)) {
        return true;
      }

      NextTokPrec = GetBinOpPrecedence(T.GetKind());
    }

    assert(NextTokPrec <= ThisPrec && "Recursion didn't work!");
  }
}