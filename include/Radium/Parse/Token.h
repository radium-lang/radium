#ifndef RADIUM_PARSE_TOKEN_H
#define RADIUM_PARSE_TOKEN_H

#include <cstdint>

#include "Radium/Basic/LLVM.h"
#include "Radium/Basic/SourceLoc.h"
#include "llvm/ADT/StringRef.h"

namespace Radium {

enum class TokenKind : uint8_t {
  unknown = 0,
  eof,
  code_complete,
  identifier,
  oper_binary,
  oper_postfix,
  oper_prefix,
  dollarident,
  integer_literal,
  floating_literal,
  string_literal,
  character_literal,
  sil_local_name,  // %42 in RIL mode.
  comment,

#define RADIUM_KEYWORD(Name) kw_##Name,
#define RADIUM_PUNCTUATOR(X, Y) X,
#include "Radium/Parse/Tokens.def"

  num_tokens,
};

class Token {
 public:
  Token() : kind_(TokenKind::num_tokens), at_start_of_line_(false) {}

  auto getKind() const -> TokenKind { return kind_; }

  void setKind(TokenKind kind) { kind_ = kind; }

  auto is(TokenKind kind) const -> bool { return kind_ == kind; }

  auto isNot(TokenKind kind) const -> bool { return kind_ != kind; }

  auto isAnyOperator() const -> bool {
    return kind_ == TokenKind::oper_binary ||
           kind_ == TokenKind::oper_postfix || kind_ == TokenKind::oper_prefix;
  }

  auto isNotAnyOperator() const -> bool { return !isAnyOperator(); }

  auto GetLocation() const -> llvm::SMLoc {
    return llvm::SMLoc::getFromPointer(text_.begin());
  }

  auto isAtStartOfLine() const -> bool { return at_start_of_line_; }

  void setAtStartOfLine(bool value) { at_start_of_line_ = value; }

  auto isContextualKeyword(llvm::StringRef context_kw) const -> bool {
    return is(TokenKind::identifier) && text_ == context_kw;
  }

  auto isContextualPunctuator(llvm::StringRef context_punc) const -> bool {
    return is(TokenKind::oper_binary) && text_ == context_punc;
  }

  auto isFollowingLParen() const -> bool {
    return !isAtStartOfLine() && kind_ == TokenKind::l_paren;
  }

  auto isFollowingLSquare() const -> bool {
    return !isAtStartOfLine() && kind_ == TokenKind::l_square;
  }

  auto isFollowingLBrace() const -> bool {
    return !isAtStartOfLine() && kind_ == TokenKind::l_brace;
  }

  auto isKeyword() const -> bool {
    switch (kind_) {
#define RADIUM_KEYWORD(X) \
  case TokenKind::kw_##X: \
    return true;
#include "Radium/Parse/Tokens.def"
      default:
        return false;
    }
  }

  auto getLoc() const -> SourceLoc {
    return SourceLoc(llvm::SMLoc::getFromPointer(text_.begin()));
  }

  auto getText() const -> llvm::StringRef { return text_; }

  void setText(llvm::StringRef text) { text_ = text; }

  auto getLength() const -> unsigned { return text_.size(); }

  void setToken(TokenKind kind, llvm::StringRef text) {
    kind_ = kind;
    text_ = text;
  }

 private:
  // token的类型
  TokenKind kind_;
  // 是否token是在行首
  bool at_start_of_line_;
  // token所在的source buffer中实际的文本
  llvm::StringRef text_;
};

}  // namespace Radium

namespace llvm {

template <typename T>
struct IsPodLike;

template <>
struct IsPodLike<Radium::Token> {
  static const bool Value = true;
};

}  // namespace llvm

#endif  // RADIUM_PARSE_TOKEN_H