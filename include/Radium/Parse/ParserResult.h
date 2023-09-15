#ifndef RADIUM_PARSE_PARSERRESULT_H
#define RADIUM_PARSE_PARSERRESULT_H

#include <type_traits>

#include "llvm/ADT/PointerIntPair.h"

namespace Radium {

class ParserStatus;

template <typename T>
class ParserResult {
 public:
  ParserResult(std::nullptr_t = nullptr) { setIsParseError(); }

  ParserResult(ParserStatus status);

  // successful results
  explicit ParserResult(T* result) : ptr_and_bits_(result) {
    assert(result && "a successful parser result can not be null");
  }

  template <typename U, typename Enabler =
                            typename std::enable_if_t<std::is_base_of_v<T, U>>>
  ParserResult(ParserResult<U> other)
      : ptr_and_bits_(other.ptr_and_bits_.getPointer(),
                      other.ptr_and_bits_.getInt()) {}

  auto getPtrOrNull() const -> T* { return ptr_and_bits_.getPointer(); }

  auto get() const -> T* {
    assert(getPtrOrNull() && "not checked for nullptr");
    return getPtrOrNull();
  }

  auto isNull() const -> bool { return getPtrOrNull() == nullptr; }

  auto isNotNull() const -> bool { return getPtrOrNull() != nullptr; }

  auto isParserError() const -> bool {
    return ptr_and_bits_.getInt() & IsError;
  }

  auto isParserErrorOrHasCompletion() const -> bool {
    return ptr_and_bits_.getInt() & (IsError | isCodeCompletion);
  }

  auto hasCodeCompletion() const -> bool {
    return ptr_and_bits_.getInt() & isCodeCompletion;
  }

  void setIsParseError() {
    ptr_and_bits_.setInt(ptr_and_bits_.getInt() | IsError);
  }

  void setHasCodeCompletionAndIsError() {
    ptr_and_bits_.setInt(ptr_and_bits_.getInt() | IsError | isCodeCompletion);
  }

 private:
  void setHasCodeCompletion() {
    ptr_and_bits_.setInt(ptr_and_bits_.getInt() | isCodeCompletion);
  }

 private:
  llvm::PointerIntPair<T*, 2> ptr_and_bits_;

  enum {
    IsError = 0x1,
    isCodeCompletion = 0x2,
  };

  template <typename U>
  friend class ParserResult;

  template <typename U>
  friend inline auto makeParserResult(ParserStatus status, U* result)
      -> ParserResult<U>;
};

template <typename T>
static inline auto makeParserResult(T* result) -> ParserResult<T> {
  return ParserResult<T>(result);
}

template <typename T>
static inline auto makeParserErrorResult(T* result = nullptr)
    -> ParserResult<T> {
  ParserResult<T> pr;
  if (result) {
    pr = ParserResult<T>(result);
  }
  pr.setHasCodeCompletionAndIsError();
  return pr;
}

class ParserStatus {
 public:
  ParserStatus() : is_error_(0), is_code_completion_(0) {}

  template <typename T>
  ParserStatus(ParserResult<T> result) : is_error_(0), is_code_completion_(0) {
    if (result.isParserError()) {
      setIsParseError();
    }
    if (result.hasCodeCompletion()) {
      is_code_completion_ = true;
    }
  }

  auto isSuccess() const -> bool { return isError(); }

  auto isError() const -> bool { return is_error_; }

  auto isErrorOrHasCompletion() const -> bool {
    return is_error_ || is_code_completion_;
  }

  auto hasCodeCompletion() const -> bool { return is_code_completion_; }

  void setIsParseError() { is_error_ = true; }

  void setHasCodeCompletion() { is_code_completion_ = true; }

  void clearIsError() { is_error_ = false; }

  void setHasCodeCompletionAndIsError() {
    is_error_ = true;
    is_code_completion_ = true;
  }

  auto operator|=(ParserStatus rhs) -> ParserStatus& {
    is_error_ |= rhs.is_error_;
    is_code_completion_ |= rhs.is_code_completion_;
    return *this;
  }

  friend auto operator|(ParserStatus lhs, ParserStatus rhs) -> ParserStatus {
    ParserStatus result = lhs;
    result |= rhs;
    return result;
  }

 private:
  unsigned is_error_ : 1;
  unsigned is_code_completion_ : 1;
};

static inline auto makeParserSuccess() -> ParserStatus {
  return ParserStatus();
}

static inline auto makeParserError() -> ParserStatus {
  ParserStatus status;
  status.setIsParseError();
  return status;
}

static inline auto makeParserCodeCompletionStatus() -> ParserStatus {
  ParserStatus status;
  status.setHasCodeCompletionAndIsError();
  return status;
}

template <typename T>
static inline auto makeParserResult(ParserStatus status, T* result)
    -> ParserResult<T> {
  ParserResult<T> pr = status.isError() ? makeParserErrorResult(result)
                                        : makeParserResult(result);

  if (status.hasCodeCompletion()) {
    pr.setHasCodeCompletion();
  }
  return pr;
}

template <typename T>
ParserResult<T>::ParserResult(ParserStatus status) {
  assert(status.isError());
  setIsParseError();
  if (status.hasCodeCompletion()) {
    setHasCodeCompletion();
  }
}

}  // namespace Radium

#endif  // RADIUM_PARSE_PARSERRESULT_H