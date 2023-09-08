#ifndef RADIUM_AST_IDENTIFIER_H
#define RADIUM_AST_IDENTIFIER_H

#include <cstring>

#include "Radium/Basic/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ConvertUTF.h"

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
};

}  // namespace Radium

#endif  // RADIUM_AST_IDENTIFIER_H