#ifndef RADIUM_AST_TYPE_H
#define RADIUM_AST_TYPE_H

#include <functional>
#include <string>

#include "Radium/AST/PrintOptions.h"
#include "Radium/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"

namespace Radium {

class ASTPrinter;
class ArchetypeType;
class CanType;
class LazyResolver;
class Module;
class TypeBase;
class Type;
class TypeWalker;
class SubstitutableType;

// 从可替换类型到其替换类型的类型替换映射。
using TypeSubstitutionMap = llvm::DenseMap<SubstitutableType*, Type>;

class Type {
  TypeBase* ptr_;

 public:
  /*implicit*/ Type(TypeBase* ptr) : ptr_(ptr) {}

  auto getPointer() const -> TypeBase* { return ptr_; }

  auto isNull() const -> bool { return ptr_ == nullptr; }

  auto operator->() const -> TypeBase* { return ptr_; }
  explicit operator bool() const { return ptr_ != nullptr; }

  auto walk(TypeWalker& walker) const -> bool;
  auto walk(TypeWalker&& walker) const -> bool { return walk(walker); }

  // 遍历给定类型及其子类型，查找给定谓词返回true的类型。
  auto findIf(const std::function<bool(Type)>& pred) const -> bool;

  // 通过将用户提供的函数应用于每个类型来转换给定的类型。
  auto transform(const std::function<Type(Type)>& fn) const -> Type;

  // 用新的具体类型替换可替换类型的引用，并返回被替换的结果。
  auto subst(Module* module, TypeSubstitutionMap& substitutions,
             bool ignore_missing, LazyResolver* resolver) const -> Type;

  void dump() const;

  void print(raw_ostream& os, const PrintOptions& po = PrintOptions()) const;
  void print(ASTPrinter& printer, const PrintOptions& po) const;

  // 以字符串形式返回类型的名称。
  auto getString(const PrintOptions& po = PrintOptions()) const -> std::string;

  auto getCanonicalTypeOrNull() const -> CanType;

 private:
  // 禁止对类型进行直接比较，因为它们可能不是标准类型。
  void operator==(Type type) const = delete;
  void operator!=(Type type) const = delete;
};

/// 这是静态并为标准（Canonical）的类型。
class CanType : public Type {
  auto isActuallyCanonicalOrNull() const -> bool;

  static auto hasReferenceSemanticsImpl(CanType type) -> bool;
  static auto isExistentialTypeImpl(CanType type) -> bool;

 public:
  explicit CanType(TypeBase* p = nullptr) : Type(p) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanType out of a non-canonical type!");
  }
  explicit CanType(Type t) : Type(t) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanType out of a non-canonical type!");
  }

  auto hasReferenceSemantics() const -> bool {
    return hasReferenceSemanticsImpl(*this);
  }

  auto isExistentialType() const -> bool {
    return isExistentialTypeImpl(*this);
  }

  auto operator==(CanType t) const -> bool {
    return getPointer() == t.getPointer();
  }
  auto operator!=(CanType t) const -> bool { return !operator==(t); }

  auto operator<(CanType t) const -> bool {
    return getPointer() < t.getPointer();
  }
};

}  // namespace Radium

#endif  // RADIUM_AST_TYPE_H