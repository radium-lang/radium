#ifndef RADIUM_AST_IDENTIFIER_H
#define RADIUM_AST_IDENTIFIER_H

#include <cstring>

#include "Radium/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/TrailingObjects.h"

namespace llvm {
class raw_ostream;
}  // namespace llvm

namespace Radium {

class ASTContext;

enum class DeclRefKind {
  /// 对标识符的普通引用, e.g. 'foo'.
  Ordinary,

  /// 作为二进制运算符对标识符的引用, e.g. '+' in 'a+b'.
  BinaryOperator,

  /// 作为后缀一元操作符对标识符的引用, e.g. '++' in 'a++'.
  PostfixOperator,

  /// 作为前缀一元操作符对标识符的引用, e.g. '--' in '--a'.
  PrefixOperator
};

// ASTContext创建的唯一标识符的实例。它只是包装了一个以空结尾的const char*。
class Identifier {
  friend class ASTContext;

  const char* pointer_;

  explicit Identifier(const char* pointer) : pointer_(pointer) {}

 public:
  explicit Identifier() : pointer_(nullptr) {}

  auto operator==(Identifier rhs) const -> bool {
    return pointer_ == rhs.pointer_;
  }
  auto operator!=(Identifier rhs) const -> bool {
    return pointer_ != rhs.pointer_;
  }

  auto operator<(Identifier rhs) const -> bool {
    return pointer_ < rhs.pointer_;
  }

  auto get() const -> const char* { return pointer_; }

  auto str() const -> llvm::StringRef { return pointer_; }

  auto getLength() const -> unsigned { return ::strlen(pointer_); }

  auto empty() const -> bool { return pointer_ == nullptr; }

  /// isOperatorStartCodePoint - Return true if the specified code point is a
  /// valid start of an operator.
  static auto isOperatorStartCodePoint(uint32_t c) -> bool {
    // ASCII operator chars.
    static const char op_chars[] = "/=-+*%<>!&|^~.?";
    if (c < 0x80) {
      return memchr(op_chars, c, sizeof(op_chars) - 1) != 0;
    }

    // Unicode math, symbol, arrow, dingbat, and line/box drawing chars.
    return (c >= 0x00A1 && c <= 0x00A7) || c == 0x00A9 || c == 0x00AB ||
           c == 0x00AC || c == 0x00AE || c == 0x00B0 || c == 0x00B1 ||
           c == 0x00B6 || c == 0x00BB || c == 0x00BF || c == 0x00D7 ||
           c == 0x00F7 || c == 0x2016 || c == 0x2017 ||
           (c >= 0x2020 && c <= 0x2027) || (c >= 0x2030 && c <= 0x203E) ||
           (c >= 0x2041 && c <= 0x2053) || (c >= 0x2055 && c <= 0x205E) ||
           (c >= 0x2190 && c <= 0x23FF) || (c >= 0x2500 && c <= 0x2775) ||
           (c >= 0x2794 && c <= 0x2BFF) || (c >= 0x2E00 && c <= 0x2E7F) ||
           (c >= 0x3001 && c <= 0x3003) || (c >= 0x3008 && c <= 0x3030);
  }

  /// isOperatorContinuationCodePoint - Return true if the specified code point
  /// is a valid operator code point.
  static auto isOperatorContinuationCodePoint(uint32_t c) -> bool {
    if (isOperatorStartCodePoint(c)) {
      return true;
    }

    // Unicode combining characters and variation selectors.
    return (c >= 0x0300 && c <= 0x036F) || (c >= 0x1DC0 && c <= 0x1DFF) ||
           (c >= 0x20D0 && c <= 0x20FF) || (c >= 0xFE00 && c <= 0xFE0F) ||
           (c >= 0xFE20 && c <= 0xFE2F) || (c >= 0xE0100 && c <= 0xE01EF);
  }
};

}  // namespace Radium

#endif  // RADIUM_AST_IDENTIFIER_H