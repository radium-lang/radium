#include "Radium/Parse/Parser.h"

#include "Radium/AST/ASTConsumer.h"
#include "Radium/AST/Decl.h"
#include "Radium/AST/Expr.h"
#include "Radium/Parse/Lexer.h"
#include "Radium/Parse/Token.h"
#include "Radium/Sema/Sema.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/SourceMgr.h"

namespace Radium {

using llvm::SMLoc;

Parser::Parser(unsigned buffer_id, ASTConsumer& consumer)
    : consumer_(consumer),
      src_mgr_(consumer.GetContext().src_mgr_),
      lexer_(*new Lexer(buffer_id, src_mgr_)),
      sema_(*new Sema(consumer.GetContext())) {}

Parser::~Parser() {
  delete &sema_;
  delete &lexer_;
}

void Parser::Note(llvm::SMLoc loc, const char* message) {
  src_mgr_.PrintMessage(loc, llvm::SourceMgr::DK_Note, message);
}

void Parser::Warning(llvm::SMLoc loc, const char* message) {
  src_mgr_.PrintMessage(loc, llvm::SourceMgr::DK_Warning, message);
}

void Parser::Error(llvm::SMLoc loc, const char* message) {
  src_mgr_.PrintMessage(loc, llvm::SourceMgr::DK_Error, message);
}

void Parser::ConsumeToken() {
  assert(token_.IsNot(Tok::TokenKind::Eof) && "Consuming past eof!");
  lexer_.Lex(token_);
}

void Parser::SkipUntil(Tok::TokenKind kind) {
  if (kind == Tok::TokenKind::Unknown) {
    return;
  }

  while (token_.IsNot(kind) && token_.IsNot(Tok::TokenKind::Eof)) {
    switch (token_.GetKind()) {
      default:
        ConsumeToken();
        break;
    }
  }
}

auto Parser::ParseIdentifier(llvm::StringRef& result, const char* message,
                             Tok::TokenKind /*skip_to_tok*/) -> bool {
  if (token_.Is(Tok::TokenKind::Identifier)) {
    result = token_.GetText();
    ConsumeToken();
    return false;
  }

  Error(token_.GetLocation(), message ? message : "Expected identifier");
  return true;
}

auto Parser::ParseToken(Tok::TokenKind kind, const char* message,
                        Tok::TokenKind skip_to_tok) -> bool {
  if (token_.Is(kind)) {
    ConsumeToken();
    return false;
  }

  Error(token_.GetLocation(), message ? message : "Expected token");
  SkipUntil(skip_to_tok);

  if (kind == skip_to_tok && token_.Is(skip_to_tok)) {
    ConsumeToken();
  }
  return true;
}

/// translation-unit ::= decl-top-level*
void Parser::ParseTranslationUnit() {
  ConsumeToken();
  while (token_.IsNot(Tok::TokenKind::Eof)) {
    Decl* decl = nullptr;
    ParseDeclTopLevel(decl);

    if (decl) {
      consumer_.HandleTopLevelDecl(decl);
    }
  }

  consumer_.HandleEndOfTranslationUnit();
}

/// decl-top-level ::= decl-var ';'
///                 |  ';'
void Parser::ParseDeclTopLevel(Decl*& result) {
  switch (token_.GetKind()) {
    default:
      Error(token_.GetLocation(), "Expected a top level declaration");
      return SkipUntil(Tok::TokenKind::Semi);
    case Tok::TokenKind::KW_var:
      ParseDeclVar(result);
      if (result) {
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
void Parser::ParseDeclVar(Decl*& result) {
  SMLoc var_loc = token_.GetLocation();
  ConsumeToken(Tok::TokenKind::KW_var);

  llvm::StringRef identifier;
  if (ParseIdentifier(identifier, "Expected identifier in var declaration")) {
    return SkipUntil(Tok::TokenKind::Semi);
  }

  Type* type = nullptr;
  if (ConsumeIf(Tok::TokenKind::Colon) &&
      ParseType(type, "Expected type in var declaration")) {
    return SkipUntil(Tok::TokenKind::Semi);
  }

  Expr* init = nullptr;
  if (ConsumeIf(Tok::TokenKind::Equal) &&
      ParseExpr(init, "Expected expression in var declaration")) {
    return SkipUntil(Tok::TokenKind::Semi);
  }

  result = sema_.ActOnVarDecl(var_loc, identifier, type, init);
}

/// type ::= 'int'
///       |  'void'
///       |  type-tuple
///       |  type '->' type
auto Parser::ParseType(Type*& result, const char* message) -> bool {
  switch (token_.GetKind()) {
    case Tok::TokenKind::KW_int:
      result = sema_.type_.ActOnIntType(token_.GetLocation());
      ConsumeToken(Tok::TokenKind::KW_int);
      return false;
    case Tok::TokenKind::KW_void:
      result = sema_.type_.ActOnVoidType(token_.GetLocation());
      ConsumeToken(Tok::TokenKind::KW_void);
      return false;
    case Tok::TokenKind::LParen:
      return ParseTypeTuple(result);
    default:
      Error(token_.GetLocation(), message ? message : "Expected type");
      return true;
  }
}

/// type-or-decl-var ::= type
///                   |  decl-var
auto Parser::ParseTypeOrDeclVar(llvm::PointerUnion<Type*, Decl*>& /*result*/,
                                const char* /*message*/) -> bool {
  // if (token_.Is(Tok::TokenKind::KW_var)) {
  //   Decl* ResultDecl = nullptr;
  //   ParseDeclVar(ResultDecl);
  //   Result = ResultDecl;
  //   return ResultDecl != nullptr;
  // }
  //
  // Type* ResultType = nullptr;
  // if (ParseType(ResultType, Message))
  //  return true;
  // Result = ResultType;
  return false;
}

/// type-tuple ::= '(' ')'
///             |  '(' type-or-decl-var (',' type-or-decl-var)* ')'
auto Parser::ParseTypeTuple(Type*& result) -> bool {
  assert(token_.Is(Tok::TokenKind::LParen) && "Not start of type tuple");
  llvm::SMLoc lp_loc = token_.GetLocation();
  ConsumeToken(Tok::TokenKind::LParen);

  llvm::SmallVector<llvm::PointerUnion<Type*, Decl*>, 8> elements;

  if (token_.IsNot(Tok::TokenKind::RParen)) {
    elements.push_back(llvm::PointerUnion<Type*, Decl*>());
    bool error = ParseTypeOrDeclVar(
        elements.back(), "Expected type or var declaration in tuple");

    // Parse (',' type-or-decl-var)*
    while (!error && token_.Is(Tok::TokenKind::Comma)) {
      ConsumeToken(Tok::TokenKind::Comma);
      elements.push_back(llvm::PointerUnion<Type*, Decl*>());
      error = ParseTypeOrDeclVar(elements.back(),
                                 "Expected type or var declaration in tuple");
    }

    if (error) {
      SkipUntil(Tok::TokenKind::RParen);
      if (token_.Is(Tok::TokenKind::RParen)) {
        ConsumeToken(Tok::TokenKind::RParen);
      }
      return true;
    }
  }

  llvm::SMLoc rp_loc = token_.GetLocation();
  if (ParseToken(Tok::TokenKind::RParen, "Expected ')' at end of tuple list",
                 Tok::TokenKind::RParen)) {
    Note(lp_loc, "to match this opening '('");
    return true;
  }

  result = sema_.type_.ActOnTupleType(lp_loc, elements.data(), elements.size(),
                                      rp_loc);
  return false;
}

/// expr ::= expr-primary expr-binary-rhs
auto Parser::ParseExpr(Expr*& result, const char* message) -> bool {
  return ParseExprPrimary(result, message) || ParseExprBinaryRHS(result);
}

/// expr-primary ::= Numeric_Constant
///               |  '(' expr ')'
auto Parser::ParseExprPrimary(Expr*& result, const char* message) -> bool {
  switch (token_.GetKind()) {
    case Tok::TokenKind::Numeric_Constant:
      result = sema_.expr_.ActOnNumericConstant(token_.GetText(),
                                                token_.GetLocation());
      ConsumeToken(Tok::TokenKind::Numeric_Constant);
      return false;
    case Tok::TokenKind::LParen: {
      llvm::SMLoc lp_loc = token_.GetLocation();
      ConsumeToken(Tok::TokenKind::LParen);
      Expr* sub_expr = nullptr;
      if (ParseExpr(sub_expr, "Expected expression in parentheses")) {
        return true;
      }

      llvm::SMLoc rp_loc = token_.GetLocation();
      if (ParseToken(Tok::TokenKind::RParen,
                     "Expected ')' at end of expression")) {
        Note(lp_loc, "to match this opening '('");
        return true;
      }

      result = sema_.expr_.ActOnParenExpr(lp_loc, sub_expr, rp_loc);
      return false;
    }
    default:
      Error(token_.GetLocation(), message ? message : "Expected expression");
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

static auto GetBinOpPrecedence(Tok::TokenKind kind) -> Prec::Level {
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

static auto GetBinOpKind(Tok::TokenKind kind) -> ExprKind {
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
auto Parser::ParseExprBinaryRHS(Expr*& result, unsigned min_prec) -> bool {
  Prec::Level next_tok_prec = GetBinOpPrecedence(token_.GetKind());
  while (true) {
    if (next_tok_prec < static_cast<Prec::Level>(min_prec)) {
      return false;
    }

    Token op_token = token_;
    ConsumeToken();

    // TODO: Support ternary operators

    Expr* leaf = nullptr;
    if (ParseExprPrimary(leaf, "Expected expression after binary operator")) {
      return true;
    }

    Prec::Level this_prec = next_tok_prec;
    next_tok_prec = GetBinOpPrecedence(token_.GetKind());

    // TODO: All operator are left associative

    if (this_prec < next_tok_prec) {
      if (ParseExprBinaryRHS(leaf, this_prec + 1)) {
        return true;
      }

      next_tok_prec = GetBinOpPrecedence(token_.GetKind());
    }

    assert(next_tok_prec <= this_prec && "Recursion didn't work!");

    result = sema_.expr_.ActOnBinaryExpr(
        static_cast<unsigned int>(GetBinOpKind(op_token.GetKind())), result,
        op_token.GetLocation(), leaf);
  }

  return false;
}

}  // namespace Radium