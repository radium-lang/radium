#ifndef RADIUM_AST_TYPES_H
#define RADIUM_AST_TYPES_H

#include "Radium/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"

namespace Radium {

class ASTContext;

enum class TypeKind {
#define TYPE(id, parent) id,
#define TYPE_RANGE(Id, FirstId, LastId) \
  First_##Id##Type = FirstId, Last_##Id##Type = LastId,
#include "Radium/AST/TypeNodes.def"
};

/// 在结构类型上递归定义的类型的各种属性。
class RecursiveTypeProperties {
 public:
  enum { BitWidth = 3 };

  enum Property : unsigned {
    HasTypeVariable = 0x01,
    IsDependent = 0x02,
    IsNotMaterializable = 0x04,
  };

 private:
  unsigned bits_ : BitWidth;

 public:
  RecursiveTypeProperties() : bits_(0) {}
  RecursiveTypeProperties(Property prop) : bits_(prop) {}
  explicit RecursiveTypeProperties(unsigned bits) : bits_(bits) {}

  /// 返回属性的位域。
  auto getBits() const -> unsigned { return bits_; }

  /// 具有这些属性的类型在结构上是否包含类型变量。
  auto hasTypeVariable() const -> bool { return bits_ & HasTypeVariable; }

  /// 具有这些属性的类型是否依赖于任何类型变量。
  auto isDependent() const -> bool { return bits_ & IsDependent; }

  /// 具有这些属性的类型是否可实例化。即是否first-class值类型。
  auto isMaterializable() const -> bool {
    return !(bits_ & IsNotMaterializable);
  }

  // 返回属性集合(任何一个集合中存在)。
  friend auto operator+(Property lhs, Property rhs) -> RecursiveTypeProperties {
    return RecursiveTypeProperties(lhs | rhs);
  }
  friend auto operator+(RecursiveTypeProperties lhs,
                        RecursiveTypeProperties rhs)
      -> RecursiveTypeProperties {
    return RecursiveTypeProperties(lhs.bits_ | rhs.bits_);
  }

  // 将右边集合中的任何属性添加到这个集合中。
  auto operator+=(RecursiveTypeProperties other) -> RecursiveTypeProperties& {
    bits_ |= other.bits_;
    return *this;
  }

  // 返回在左侧集合中存在但在右侧集合中不存在的属性集。
  auto operator-(RecursiveTypeProperties other) -> RecursiveTypeProperties {
    return RecursiveTypeProperties(bits_ & ~other.bits_);
  }

  // 从这个集合中删除右边集合中的任何属性。
  auto operator-=(RecursiveTypeProperties other) -> RecursiveTypeProperties& {
    bits_ &= ~other.bits_;
    return *this;
  }

  // 用于集合的属性验证。
  auto operator&(Property prop) const -> bool { return bits_ & prop; }
};

/// Radium中所有类型的基类。
class alignas(8) TypeBase {
  // alignas(8)是因为在Type上需要三个标签位。

  friend class ASTContext;
  TypeBase(const TypeBase&) = delete;
  void operator=(const TypeBase&) = delete;

  // 对于标准（canonical）类型，该字段总是被设置为ASTContext，
  // 当请求非标准类型的标准形式时，该字段将被ASTContext惰性填充。
  llvm::PointerUnion<TypeBase*, const ASTContext*> canonical_type_;

  // 表示该类型是哪个子类的鉴别符。
  const TypeKind kind_;

  struct TypeBaseBitfields {
    unsigned properties : RecursiveTypeProperties::BitWidth;
  };

  enum { NumTypeBaseBits = RecursiveTypeProperties::BitWidth };
  static_assert(NumTypeBaseBits <= 32, "fits in an unsigned");

 protected:
  // 函数类型位域。
  struct AnyFunctionTypeBitfields {
    unsigned : NumTypeBaseBits;

    // 影响函数调用方式的额外信息，比如regparm和调用约定。
    unsigned ext_info : 8;
  };
  enum { NumAnyFunctionTypeBits = NumTypeBaseBits + 8 };
  static_assert(NumAnyFunctionTypeBits <= 32, "fits in an unsigned");

  // 类型变量类型位域。
  struct TypeVariableTypeBitfields {
    unsigned : NumTypeBaseBits;

    // 分配给此类型变量的唯一编号
    unsigned id : 32 - NumTypeBaseBits;
  };
  enum { NumTypeVariableTypeBits = NumTypeBaseBits + (32 - NumTypeBaseBits) };
  static_assert(NumTypeVariableTypeBits <= 32, "fits in an unsigned");

  // Radium IL函数类型位域。
  struct RILFunctionTypeBitfields {
    unsigned : NumTypeBaseBits;
    unsigned ext_info : 8;
    unsigned call_ee_convention : 3;
    unsigned num_parameters : 32 - 11 - NumTypeBaseBits;
  };

  // 元类型位域。
  struct MetatypeTypeBitfields {
    unsigned : NumTypeBaseBits;
    // 是否thin被设置。
    unsigned has_thin : 1;
    // 元类型是否使用thin表示。
    unsigned thin : 1;
  };
  enum { NumMetatypeTypeBits = NumTypeBaseBits + 2 };
  static_assert(NumMetatypeTypeBits <= 32, "fits in an unsigned");

  union {
    TypeBaseBitfields TypeBaseBits;
    AnyFunctionTypeBitfields AnyFunctionTypeBits;
    TypeVariableTypeBitfields TypeVariableTypeBits;
    RILFunctionTypeBitfields SILFunctionTypeBits;
    MetatypeTypeBitfields MetatypeTypeBits;
  };

 protected:
  TypeBase(TypeKind kind, const ASTContext* can_type_ctx,
           RecursiveTypeProperties properties)
      : canonical_type_((TypeBase*)nullptr), kind_(kind) {
    if (can_type_ctx) {
      canonical_type_ = can_type_ctx;
    }
    setRecursiveProperties(properties);
  }

  void setRecursiveProperties(RecursiveTypeProperties properties) {
    TypeBaseBits.properties = properties.getBits();
  }

 public:
  auto getKind() const -> TypeKind { return kind_; }

  auto isCanonical() const -> bool {
    return canonical_type_.is<const ASTContext*>();
  }

  auto hasCanonicalTypeComputed() const -> bool {
    return !canonical_type_.isNull();
  }

  auto getCanonicalType() const -> CanType;

  auto getASTContext() const -> ASTContext& {
    // 如果类型是canonical的，那么它的ASTContext就是canonical的。
    if (canonical_type_.is<const ASTContext*>()) {
      return const_cast<ASTContext&>(*canonical_type_.get<const ASTContext*>());
    }
    // 否则，需要canonical类型的ASTContext。
    return const_cast<ASTContext&>(
        *getCanonicalType()->canonical_type_.get<const ASTContext*>());
  }

  auto isEqual(Type other) const -> bool;

  auto isSpelledLike(Type other) const -> bool;

  auto getDesugaredType() -> TypeBase*;

  template <typename T>
  auto getAs() -> T* {
    return dyn_cast<T>(getDesugaredType());
  }

  template <typename T>
  auto is() -> bool {
    return isa<T>(getDesugaredType());
  }

  template <typename T>
  auto castTo() -> T* {
    return cast<T>(getDesugaredType());
  }
};

/// 代表类型被错误构造。
class ErrorType : public TypeBase {
  friend class ASTContext;

  ErrorType(ASTContext& ctx)
      : TypeBase(TypeKind::Error, &ctx, RecursiveTypeProperties()) {}

 public:
  static auto get(const ASTContext& ctx) -> Type;

  static auto classof(const TypeBase* type) -> bool {
    return type->getKind() == TypeKind::Error;
  }
};

}  // namespace Radium

#endif  // RADIUM_AST_TYPES_H