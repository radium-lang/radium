#include "radium/Parse/Parser.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/SourceMgr.h"
#include "radium/AST/ASTConsumer.h"
#include "radium/AST/Decl.h"
#include "radium/AST/Expr.h"
#include "radium/Parse/Lexer.h"
#include "radium/Parse/Token.h"
#include "radium/Sema/Sema.h"

using namespace Radium;
using llvm::SMLoc;

Parser::Parser(unsigned BufferID, ASTConsumer& Consumer)
    : Consumer(Consumer),
      SrcMgr(Consumer.GetContext().SrcMgr),
      L(*new Lexer(BufferID, SrcMgr)),
      S(*new Sema(Consumer.GetContext())) {}

Parser::~Parser() {
  delete &S;
  delete &L;
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
  L.Lex(T);
}

void Parser::SkipUntil(Tok::TokenKind K) {
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
  SkipUntil(SkipToTok);

  if (K == SkipToTok && T.Is(SkipToTok)) {
    ConsumeToken();
  }
  return true;
}

/// translation-unit ::= decl-top-level*
void Parser::ParseTranslationUnit() {
  ConsumeToken();
  while (T.IsNot(Tok::TokenKind::Eof)) {
    Decl* D = nullptr;
    ParseDeclTopLevel(D);

    if (D)
      Consumer.HandleTopLevelDecl(D);
  }

  Consumer.HandleEndOfTranslationUnit();
}

/// decl-top-level ::= decl-var ';'
///                 |  ';'
void Parser::ParseDeclTopLevel(Decl*& Result) {
  switch (T.GetKind()) {
    default:
      Error(T.GetLocation(), "Expected a top level declaration");
      return SkipUntil(Tok::TokenKind::Semi);
    case Tok::TokenKind::KW_var:
      ParseDeclVar(Result);
      if (Result) {
        ParseToken(Tok::TokenKind::Semi,
                   "Expected ';' at end of var declaration",
                   Tok::TokenKind::Semi);
        return;
      }
    case Tok::TokenKind::Semi:
      ConsumeToken(Tok::TokenKind::Semi);
  }
}

/// decl-var ::= 'var' identifier ':' type
///           |  'var' identifier ':' type '=' expr
///           |  'var' identifier ':' '=' expr
void Parser::ParseDeclVar(Decl*& Result) {
  SMLoc VarLoc = T.GetLocation();
  ConsumeToken(Tok::TokenKind::KW_var);

  llvm::StringRef Identifier;
  if (ParseIdentifier(Identifier, "Expected identifier in var declaration"))
    return SkipUntil(Tok::TokenKind::Semi);

  Type* Ty = nullptr;
  if (ConsumeIf(Tok::TokenKind::Colon) &&
      ParseType(Ty, "Expected type in var declaration")) {
    return SkipUntil(Tok::TokenKind::Semi);
  }

  Expr* Init = nullptr;
  if (ConsumeIf(Tok::TokenKind::Equal) &&
      ParseExpr(Init, "Expected expression in var declaration")) {
    return SkipUntil(Tok::TokenKind::Semi);
  }

  Result = S.ActOnVarDecl(VarLoc, Identifier, Ty, Init);
}

/// type ::= 'int'
///       |  'void'
///       |  type-tuple
///       |  type '->' type
bool Parser::ParseType(Type*& Result, const char* Message) {
  switch (T.GetKind()) {
    case Tok::TokenKind::KW_int:
      Result = S.type.ActOnIntType(T.GetLocation());
      ConsumeToken(Tok::TokenKind::KW_int);
      return false;
    case Tok::TokenKind::KW_void:
      Result = S.type.ActOnVoidType(T.GetLocation());
      ConsumeToken(Tok::TokenKind::KW_void);
      return false;
    case Tok::TokenKind::LParen:
      return ParseTypeTuple(Result);
    default:
      Error(T.GetLocation(), Message ? Message : "Expected type");
      return true;
  }
}

/// type-or-decl-var ::= type
///                   |  decl-var
bool Parser::ParseTypeOrDeclVar(llvm::PointerUnion<Type*, Decl*>& Result,
                                const char* Message) {
  if (T.Is(Tok::TokenKind::KW_var)) {
    Decl* ResultDecl = nullptr;
    ParseDeclVar(ResultDecl);
    Result = ResultDecl;
    return ResultDecl != nullptr;
  }

  Type* ResultType = nullptr;
  if (ParseType(ResultType, Message))
    return true;
  Result = ResultType;
  return false;
}

/// type-tuple ::= '(' ')'
///             |  '(' type-or-decl-var (',' type-or-decl-var)* ')'
bool Parser::ParseTypeTuple(Type*& Result) {
  assert(T.Is(Tok::TokenKind::LParen) && "Not start of type tuple");
  llvm::SMLoc LPLoc = T.GetLocation();
  ConsumeToken(Tok::TokenKind::LParen);

  llvm::SmallVector<llvm::PointerUnion<Type*, Decl*>, 8> Elements;

  if (T.IsNot(Tok::TokenKind::RParen)) {
    Elements.push_back(llvm::PointerUnion<Type*, Decl*>());
    bool Error = ParseTypeOrDeclVar(
        Elements.back(), "Expected type or var declaration in tuple");

    // Parse (',' type-or-decl-var)*
    while (!Error && T.Is(Tok::TokenKind::Comma)) {
      ConsumeToken(Tok::TokenKind::Comma);
      Elements.push_back(llvm::PointerUnion<Type*, Decl*>());
      Error = ParseTypeOrDeclVar(Elements.back(),
                                 "Expected type or var declaration in tuple");
    }

    if (Error) {
      SkipUntil(Tok::TokenKind::RParen);
      if (T.Is(Tok::TokenKind::RParen)) {
        ConsumeToken(Tok::TokenKind::RParen);
      }
      return true;
    }
  }

  llvm::SMLoc RPLoc = T.GetLocation();
  if (ParseToken(Tok::TokenKind::RParen, "Expected ')' at end of tuple list",
                 Tok::TokenKind::RParen)) {
    Note(LPLoc, "to match this opening '('");
    return true;
  }

  Result =
      S.type.ActOnTupleType(LPLoc, Elements.data(), Elements.size(), RPLoc);
  return false;
}

/// expr ::= expr-primary expr-binary-rhs
bool Parser::ParseExpr(Expr*& Result, const char* Message) {
  return ParseExprPrimary(Result, Message) || ParseExprBinaryRHS(Result);
}

/// expr-primary ::= Numeric_Constant
///               |  '(' expr ')'
bool Parser::ParseExprPrimary(Expr*& Result, const char* Message) {
  switch (T.GetKind()) {
    case Tok::TokenKind::Numeric_Constant:
      Result = S.expr.ActOnNumericConstant(T.GetText(), T.GetLocation());
      ConsumeToken(Tok::TokenKind::Numeric_Constant);
      return false;
    case Tok::TokenKind::LParen: {
      llvm::SMLoc LPLoc = T.GetLocation();
      ConsumeToken(Tok::TokenKind::LParen);
      Expr* SubExpr = nullptr;
      if (ParseExpr(SubExpr, "Expected expression in parentheses"))
        return true;

      llvm::SMLoc RPLoc = T.GetLocation();
      if (ParseToken(Tok::TokenKind::RParen,
                     "Expected ')' at end of expression")) {
        Note(LPLoc, "to match this opening '('");
        return true;
      }

      Result = S.expr.ActOnParenExpr(LPLoc, SubExpr, RPLoc);
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

static ExprKind GetBinOpKind(Tok::TokenKind kind) {
  switch (kind) {
    default:
      assert(0 && "not a binary operator!");
    case Tok::TokenKind::Plus:
      return ExprKind::BinaryAddExprKind;
    case Tok::TokenKind::Minus:
      return ExprKind::BinarySubExprKind;
    case Tok::TokenKind::Slash:
      return ExprKind::BinaryDivExprKind;
    case Tok::TokenKind::Star:
      return ExprKind::BinaryMulExprKind;
  }
}

/// expr-binary-rhs ::= (binary-op expr-primary)*
bool Parser::ParseExprBinaryRHS(Expr*& Result, unsigned MinPrec) {
  Prec::Level NextTokPrec = GetBinOpPrecedence(T.GetKind());
  while (true) {
    if (NextTokPrec < (Prec::Level)MinPrec)
      return false;

    Token OpToken = T;
    ConsumeToken();

    // TODO: Support ternary operators

    Expr* Leaf = nullptr;
    if (ParseExprPrimary(Leaf, "Expected expression after binary operator"))
      return true;

    Prec::Level ThisPrec = NextTokPrec;
    NextTokPrec = GetBinOpPrecedence(T.GetKind());

    // TODO: All operator are left associative

    if (ThisPrec < NextTokPrec) {
      if (ParseExprBinaryRHS(Leaf, ThisPrec + 1)) {
        return true;
      }

      NextTokPrec = GetBinOpPrecedence(T.GetKind());
    }

    assert(NextTokPrec <= ThisPrec && "Recursion didn't work!");

    Result = S.expr.ActOnBinaryExpr(
        static_cast<unsigned int>(GetBinOpKind(OpToken.GetKind())), Result,
        OpToken.GetLocation(), Leaf);
  }

  return false;
}