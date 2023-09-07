#ifndef RADIUM_BASIC_SOURCELOC_H
#define RADIUM_BASIC_SOURCELOC_H

#include "Radium/Basic/LLVM.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

namespace Radium {

class SourceManager;

/// SMLoc的封装，为了加强代码库纯度，移除了SMLoc中`getFromPointer`接口。
class SourceLoc {
  friend class SourceManager;
  friend class SourceRange;

  llvm::SMLoc loc_;

 public:
  SourceLoc() = default;
  explicit SourceLoc(const llvm::SMLoc& loc) : loc_(loc) {}

  auto isValid() const -> bool { return loc_.isValid(); }
  auto isInvalid() const -> bool { return !loc_.isValid(); }

  auto operator==(const SourceLoc& rhs) const -> bool {
    return loc_ == rhs.loc_;
  }
  auto operator!=(const SourceLoc& rhs) const -> bool {
    return loc_ != rhs.loc_;
  }

  /// 返回超出指定字节数的源位置。
  auto getAdvancedLoc(int byte_offset) const -> SourceLoc {
    assert(isValid() && "Cannot advance an invalid location");
    return SourceLoc(
        llvm::SMLoc::getFromPointer(loc_.getPointer() + byte_offset));
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
  auto isInvalid() const -> bool { return start.isInvalid(); }

  void print(llvm::raw_ostream& os, const SourceManager& sm,
             unsigned& last_buffer_id, bool print_text = true) const;

  void print(llvm::raw_ostream& os, const SourceManager& sm,
             bool print_text = true) const {
    unsigned tmp = ~0U;
    print(os, sm, tmp, print_text);
  }

  void dump(const SourceManager& sm) const;
};

}  // namespace Radium

#endif  // RADIUM_BASIC_SOURCELOC_H