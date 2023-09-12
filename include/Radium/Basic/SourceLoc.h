#ifndef RADIUM_BASIC_SOURCELOC_H
#define RADIUM_BASIC_SOURCELOC_H

#include "Radium/Basic/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

namespace Radium {

class SourceManager;

/// SMLoc的封装，为了加强代码库纯度，移除了SMLoc中`getFromPointer`接口。
class SourceLoc {
  friend class SourceManager;
  friend class SourceRange;
  friend class CharSourceRange;

  llvm::SMLoc loc_;

 public:
  SourceLoc() = default;
  explicit SourceLoc(const llvm::SMLoc& loc) : loc_(loc) {}

  auto isValid() const -> bool { return loc_.isValid(); }
  auto isInvalid() const -> bool { return !loc_.isValid(); }

  /// 用于if语句的转换。
  /// 例如：if (auto x = getSourceLoc()) { ... }
  explicit operator bool() const { return isValid(); }

  auto operator==(const SourceLoc& rhs) const -> bool {
    return loc_ == rhs.loc_;
  }
  auto operator!=(const SourceLoc& rhs) const -> bool {
    return !operator==(rhs);
  }

  /// 返回超出指定字节数的源位置。
  auto getAdvancedLoc(int byte_offset) const -> SourceLoc {
    assert(isValid() && "Cannot advance an invalid location");
    return SourceLoc(
        llvm::SMLoc::getFromPointer(loc_.getPointer() + byte_offset));
  }

  auto getAdvancedLocOrInvalid(int byte_offset) const -> SourceLoc {
    if (isValid()) {
      return getAdvancedLoc(byte_offset);
    }
    return SourceLoc();
  }

  auto getOpaquePointerValue() const -> const void* {
    return loc_.getPointer();
  }

  /// 打印SourceLoc对象，如果位置在用last_buffer_id标记的buffer中，
  /// 则不打印文件名，否则打印。
  void print(llvm::raw_ostream& os, const SourceManager& src_mgr,
             unsigned& last_buffer_id) const;

  void printLineAndColumn(llvm::raw_ostream& os,
                          const SourceManager& src_mgr) const;

  void print(llvm::raw_ostream& os, const SourceManager& src_mgr) const {
    unsigned tmp = ~0U;
    print(os, src_mgr, tmp);
  }

  void dump(const SourceManager& src_mgr) const;

  friend auto hash_value(SourceLoc loc) -> size_t {
    return reinterpret_cast<uintptr_t>(loc.getOpaquePointerValue());
  }
};

/// SourceRange代表一对位置。
class SourceRange {
 public:
  SourceLoc start, end;

  SourceRange() = default;
  SourceRange(SourceLoc loc) : start(loc), end(loc) {}
  SourceRange(SourceLoc start, SourceLoc end) : start(start), end(end) {
    assert(start.isValid() == end.isValid() &&
           "start and end should either both be valid or both be invalid!");
  }

  auto isValid() const -> bool { return start.isValid(); }
  auto isInvalid() const -> bool { return !isValid(); }

  explicit operator bool() const { return isValid(); }

  /// 将此SourceRange扩展到最小的连续SourceRange，该SourceRange包含此SourceRange和other。
  void widen(SourceRange other);

  /// 检查此范围是否包含给定位置。
  auto contains(SourceLoc loc) const -> bool;

  /// 检查此范围是否与给定范围重叠。
  auto overlaps(SourceRange other) const -> bool;

  auto operator==(const SourceRange& other) const -> bool {
    return start == other.start && end == other.end;
  }
  auto operator!=(const SourceRange& other) const -> bool {
    return !operator==(other);
  }

  void print(llvm::raw_ostream& os, const SourceManager& sm,
             unsigned& last_buffer_id, bool print_text = true) const;

  void print(llvm::raw_ostream& os, const SourceManager& sm,
             bool print_text = true) const {
    unsigned tmp = ~0U;
    print(os, sm, tmp, print_text);
  }

  void dump(const SourceManager& sm) const;
};

/// 半开放的基于字符的源范围：[start, start + byte_length)。
class CharSourceRange {
 public:
  CharSourceRange() = default;

  CharSourceRange(SourceLoc loc, unsigned byte_length)
      : start_(loc), byte_length_(byte_length) {}

  CharSourceRange(const SourceManager& sm, SourceLoc start, SourceLoc end);

  CharSourceRange(const SourceManager& sm, SourceRange range) = delete;

  auto isValid() const -> bool { return start_.isValid(); }
  auto isInvalid() const -> bool { return !isValid(); }

  auto operator==(const CharSourceRange& other) const -> bool {
    return start_ == other.start_ && byte_length_ == other.byte_length_;
  }
  auto operator!=(const CharSourceRange& other) const -> bool {
    return !operator==(other);
  }

  auto getStart() const -> SourceLoc { return start_; }
  auto getEnd() const -> SourceLoc {
    return start_.getAdvancedLocOrInvalid(byte_length_);
  }

  /// 如果给定的源位置包含在范围内，则返回true。
  auto contains(SourceLoc loc) const -> bool {
    auto less = std::less<const char*>();
    auto less_equal = std::less_equal<const char*>();
    return less_equal(getStart().loc_.getPointer(), loc.loc_.getPointer()) &&
           less(loc.loc_.getPointer(), getEnd().loc_.getPointer());
  }

  auto contains(CharSourceRange other) const -> bool {
    auto less_equal = std::less_equal<const char*>();
    return contains(other.getStart()) &&
           less_equal(other.getEnd().loc_.getPointer(),
                      getEnd().loc_.getPointer());
  }

  /// 如果other的范围大于*this的，则将*this扩展到other。
  void widen(CharSourceRange other) {
    auto diff = other.getEnd().loc_.getPointer() - getEnd().loc_.getPointer();
    if (diff > 0) {
      byte_length_ += diff;
    }
    const auto* const start_ptr = getStart().loc_.getPointer();
    diff = start_ptr - other.getStart().loc_.getPointer();
    if (diff > 0) {
      start_ = SourceLoc(llvm::SMLoc::getFromPointer(start_ptr - diff));
      byte_length_ += diff;
    }
  }

  auto overlaps(CharSourceRange other) const -> bool {
    if (getByteLength() == 0 || other.getByteLength() == 0) {
      return false;
    }
    return contains(other.getStart()) || other.contains(getStart());
  }

  auto str() const -> llvm::StringRef {
    return llvm::StringRef(getStart().loc_.getPointer(), getByteLength());
  }

  auto getByteLength() const -> unsigned {
    assert(isValid() && "length does not make sense for an invalid range");
    return byte_length_;
  }

  void print(llvm::raw_ostream& os, const SourceManager& sm,
             unsigned& last_buffer_id, bool print_text = true) const;

  void print(llvm::raw_ostream& os, const SourceManager& sm,
             bool print_text = true) const {
    unsigned tmp = ~0U;
    print(os, sm, tmp, print_text);
  }

  void dump(const SourceManager& src_mgr) const;

 private:
  SourceLoc start_;
  unsigned byte_length_;
};

}  // namespace Radium

namespace llvm {
template <typename T, typename Enable>
struct DenseMapInfo;

template <>
struct DenseMapInfo<Radium::SourceLoc> {
  static auto getEmptyKey() -> Radium::SourceLoc {
    return Radium::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char*>::getEmptyKey()));
  }

  static auto getTombstoneKey() -> Radium::SourceLoc {
    return Radium::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char*>::getTombstoneKey()));
  }

  static auto getHashValue(const Radium::SourceLoc& val) -> unsigned {
    return DenseMapInfo<const void*>::getHashValue(val.getOpaquePointerValue());
  }

  static auto isEqual(const Radium::SourceLoc& lhs,
                      const Radium::SourceLoc& rhs) -> bool {
    return lhs == rhs;
  }
};

template <>
struct DenseMapInfo<Radium::SourceRange> {
  static auto getEmptyKey() -> Radium::SourceRange {
    return Radium::SourceRange(Radium::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char*>::getEmptyKey())));
  }

  static auto getTombstoneKey() -> Radium::SourceRange {
    return Radium::SourceRange(Radium::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char*>::getTombstoneKey())));
  }

  static auto getHashValue(const Radium::SourceRange& val) -> unsigned {
    return hash_combine(val.start.getOpaquePointerValue(),
                        val.end.getOpaquePointerValue());
  }

  static auto isEqual(const Radium::SourceRange& lhs,
                      const Radium::SourceRange& rhs) -> bool {
    return lhs == rhs;
  }
};
}  // namespace llvm

#endif  // RADIUM_BASIC_SOURCELOC_H