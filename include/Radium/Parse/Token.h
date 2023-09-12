#ifndef RADIUM_PARSE_TOKEN_H
#define RADIUM_PARSE_TOKEN_H

#include <cstdint>

#include "Radium/Basic/LLVM.h"
#include "Radium/Basic/SourceLoc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"

namespace Radium {

enum class TokenKind : uint8_t {
#define RADIUM_TOKEN(Name) Name,
#include "Radium/Parse/TokenKinds.def"

  num_tokens,
};

auto getTokenText(TokenKind kind) -> llvm::StringRef;

class Token {
 public:
  Token(TokenKind kind, llvm::StringRef text, unsigned comment_length = 0)
      : kind_(kind),
        at_start_of_line_(false),
        escaped_identifier_(false),
        multiline_string_(false),
        custom_delimiter_len_(0),
        comment_length_(comment_length),
        text_(text) {}

  Token() : Token(TokenKind::num_tokens, {}, 0) {}

  auto getKind() const -> TokenKind { return kind_; }
  void setKind(TokenKind kind) { kind_ = kind; }
  void clearCommentLength() { comment_length_ = 0; }

  auto is(TokenKind kind) const -> bool { return kind_ == kind; }
  auto isNot(TokenKind kind) const -> bool { return kind_ != kind; }
  auto isAny(TokenKind k1) const -> bool { return is(k1); }
  template <typename... Ts>
  auto isAny(TokenKind k1, TokenKind k2, Ts... ks) const -> bool {
    return is(k1) || isAny(k2, ks...);
  }
  template <typename... Ts>
  auto isNot(TokenKind k1, Ts... ks) const -> bool {
    return !isAny(k1, ks...);
  }

  auto isBinaryOperator() const -> bool {
    return kind_ == TokenKind::oper_binary_spaced ||
           kind_ == TokenKind::oper_binary_unspaced;
  }

  /// 中缀 '=', '?', '->'
  auto isBinaryOperatorLike() const -> bool {
    if (isBinaryOperator()) {
      return true;
    }

    switch (kind_) {
      case TokenKind::equal:
      case TokenKind::question_infix:
      case TokenKind::arrow:
        return true;
      default:
        return false;
    }
    llvm_unreachable("Unhandled case in switch!");
  }

  /// 后缀 '!' and '?'
  auto isPostfixOperatorLike() const -> bool {
    switch (kind_) {
      case TokenKind::oper_postfix:
      case TokenKind::exclaim_postfix:
      case TokenKind::question_postfix:
        return true;
      default:
        return false;
    }
    llvm_unreachable("Unhandled case in switch!");
  }

  auto isAnyOperator() const -> bool {
    return isBinaryOperator() || kind_ == TokenKind::oper_postfix ||
           kind_ == TokenKind::oper_prefix;
  }
  auto isNotAnyOperator() const -> bool { return !isAnyOperator(); }

  auto isEllipsis() const -> bool { return isAnyOperator() && text_ == "..."; }
  auto isNotEllipsis() const -> bool { return !isEllipsis(); }

  auto isTilde() const -> bool { return isAnyOperator() && text_ == "~"; }

  auto GetLocation() const -> llvm::SMLoc {
    return llvm::SMLoc::getFromPointer(text_.begin());
  }

  auto isAtStartOfLine() const -> bool { return at_start_of_line_; }

  void setAtStartOfLine(bool value) { at_start_of_line_ = value; }

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
#include "Radium/Parse/TokenKinds.def"
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

  void setToken(TokenKind kind, llvm::StringRef text, unsigned comment_length) {
    kind_ = kind;
    text_ = text;
    comment_length_ = comment_length;
    escaped_identifier_ = false;
    multiline_string_ = false;
    custom_delimiter_len_ = 0;
  }

  auto getCommentStart() const -> SourceLoc {
    if (comment_length_ == 0) {
      return SourceLoc();
    }
    return SourceLoc(llvm::SMLoc::getFromPointer(trimComment().begin()));
  }

  auto hasComment() const -> bool { return comment_length_ != 0; }

 private:
  auto trimComment() const -> llvm::StringRef {
    assert(hasComment() && "No comment to trim");
    llvm::StringRef raw(text_.begin() - comment_length_, comment_length_);
    return raw.trim();
  }

 private:
  // token的类型
  TokenKind kind_;
  // token是否是在行首
  bool at_start_of_line_ : 1;
  unsigned escaped_identifier_ : 1;
  unsigned multiline_string_ : 1;
  // 字符串字面量的自定义分隔符的长度
  unsigned custom_delimiter_len_ : 8;
  unsigned comment_length_;
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