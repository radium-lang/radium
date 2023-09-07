#ifndef RADIUM_BASIC_OPTIONAL_H
#define RADIUM_BASIC_OPTIONAL_H

#include <cassert>
#include <type_traits>
#include <utility>

namespace Radium {

static const enum class Nothing_t { Nothing } Nothing = Nothing_t::Nothing;

static_assert(!std::is_convertible_v<Nothing_t, bool>,
              "Nothing must not be implicitly convertible to bool");

template <typename T>
class Optional {
  // 将值放置在匿名联合中以抑制隐式值语义（联合中
  // 必须显式调用构造和析构来存储一个对象）。
  union {
    T value_;
  };
  unsigned has_value_ : 1;

 public:
  Optional() : has_value_(false) {}

  Optional(Nothing_t) : has_value_(false) {}

  template <typename... ArgTypes>
  Optional(ArgTypes&&... args)
      : value_(std::forward<ArgTypes>(args)...), has_value_(true) {}

  Optional(Optional& other) : has_value_(other.has_value_) {
    if (has_value_) {
      ::new ((void*)&value_) T(other.value_);
    }
  }

  Optional(const Optional& other) : has_value_(other.has_value_) {
    if (has_value_) {
      ::new ((void*)&value_) T(other.value_);
    }
  }

  Optional(Optional&& other) : has_value_(other.has_value_) {
    if (has_value_) {
      ::new ((void*)&value_) T(std::move(other.value_));
      other.has_value_ = false;
    }
  }

  auto operator=(const Optional& other) -> Optional& {
    if (has_value_ && other.has_value_) {
      value_ = other.value_;
      return *this;
    }

    if (has_value_) {
      reset();
      return *this;
    }

    if (other.has_value_) {
      ::new ((void*)&value_) T(other.value_);
      has_value_ = true;
      other.reset();
    }

    return *this;
  }

  ~Optional() { reset(); }

  /// 根据参数in place构造新对象。
  template <typename... ArgTypes>
  void emplace(ArgTypes&&... args) {
    reset();
    ::new ((void*)&value_) T(std::forward<ArgTypes>(args)...);
    has_value_ = true;
  }

  void reset() {
    if (!has_value_) {
      return;
    }
    value_.~T();
    has_value_ = false;
  }

  auto getValue() & -> T& {
    assert(has_value_);
    return value_;
  }

  auto getValue() const& -> const T& {
    assert(has_value_);
    return value_;
  }

  auto getValue() && -> T {
    assert(has_value_);
    T result = std::move(value_);
    reset();
    return result;
  }

  auto getValueOr(T& default_value) & -> T& {
    if (has_value_) {
      return value_;
    }
    return default_value;
  }

  auto hasValue() const -> bool { return has_value_; }

  explicit operator bool() const { return has_value_; }

  auto operator->() const -> const T* {
    assert(has_value_);
    return &value_;
  }

  auto operator->() -> T* {
    assert(has_value_);
    return &value_;
  }

  auto operator*() const& -> const T& {
    assert(has_value_);
    return value_;
  }

  auto operator*() & -> T& {
    assert(has_value_);
    return value_;
  }

  auto operator*() && -> T {
    assert(has_value_);
    T result = std::move(value_);
    reset();
    return result;
  }

  /// Optional内有值返回值，无值执行对应函数并存储函数返回值。
  template <typename NullaryFunctor>
  auto cache(NullaryFunctor&& f) -> const T& {
    if (hasValue()) {
      return getValue();
    }
    emplace(f());
    return getValue();
  }
};

}  // namespace Radium

#endif  // RADIUM_BASIC_OPTIONAL_H