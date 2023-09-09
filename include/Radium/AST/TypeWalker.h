#ifndef RADIUM_AST_TYPEWALKER_H
#define RADIUM_AST_TYPEWALKER_H

#include "Radium/AST/Type.h"

namespace Radium {

/// 遍历Type的抽象类。
class TypeWalker {
 public:
  enum class Action {
    /// 继续遍历子类型。
    Continue,

    /// 停止遍历子类型。
    SkipChildren,

    /// 停止遍历整个类型。
    Stop,
  };

  /// 此方法在第一次访问类型时调用，然后再进入其子类型。
  virtual auto walkToTypePre(Type* /*type*/) -> Action {
    return Action::Continue;
  }

  /// 此方法在访问类型的子类型后调用。
  virtual auto walkToTypePost(Type* /*type*/) -> Action {
    return Action::Continue;
  }

 protected:
  TypeWalker() = default;
  TypeWalker(const TypeWalker&) = default;
  virtual ~TypeWalker() = default;

  virtual auto anchor() -> void;
};

}  // namespace Radium

#endif  // RADIUM_AST_TYPEWALKER_H