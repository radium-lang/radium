#ifndef RADIUM_BASIC_SOURCEMANAGER_H
#define RADIUM_BASIC_SOURCEMANAGER_H

#include "Radium/Basic/SourceLoc.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SourceMgr.h"

namespace Radium {

// TODO: 改造为llvm vfs管理。
/// 用于管理source buffer。
class SourceManager {
  llvm::SourceMgr llvm_src_mgr_;
  unsigned code_completion_buffer_id_ = ~0U;
  unsigned code_completion_offset_;
  // 使用了hashbang line `#!`的buffer id。
  unsigned hash_bang_buffer_id_ = ~0U;
  llvm::StringMap<unsigned> buffer_id_map_;

 public:
  SourceManager() = default;

  auto operator->() -> llvm::SourceMgr* { return &llvm_src_mgr_; }
  auto operator->() const -> const llvm::SourceMgr* { return &llvm_src_mgr_; }

  auto getLLVMSourceMgr() const -> const llvm::SourceMgr& {
    return llvm_src_mgr_;
  }

  void setCodeCompletionPoint(unsigned buffer_id, unsigned offset) {
    assert(buffer_id != ~0U && "Invalid buffer id");
    code_completion_buffer_id_ = buffer_id;
    code_completion_offset_ = offset;
  }

  auto getCodeCompletionBufferID() const -> unsigned {
    return code_completion_buffer_id_;
  }

  auto getCodeCompletionOffset() const -> unsigned {
    return code_completion_offset_;
  }

  auto getCodeCompletionLoc() const -> SourceLoc;

  void setHashbangBufferID(unsigned buffer_id) {
    assert(hash_bang_buffer_id_ == ~0U && "Hashbang buffer ID already set");
    hash_bang_buffer_id_ = buffer_id;
  }

  auto getHashbangBufferID() const -> unsigned { return hash_bang_buffer_id_; }

  // 如果在source buffer中lhs在rhs之前，则返回true。
  auto isBeforeInBuffer(SourceLoc lhs, SourceLoc rhs) const -> bool {
    return lhs.loc_.getPointer() < rhs.loc_.getPointer();
  }

  // 如果range包含loc，则返回true。
  auto rangeContainsTokenLoc(SourceRange range, SourceLoc loc) const -> bool {
    return loc == range.start || loc == range.end ||
           (isBeforeInBuffer(range.start, loc) &&
            isBeforeInBuffer(loc, range.end));
  }

  // 如果enclosing包含inner，则返回true。
  auto rangeContains(SourceRange enclosing, SourceRange inner) const -> bool {
    return rangeContainsTokenLoc(enclosing, inner.start) &&
           rangeContainsTokenLoc(enclosing, inner.end);
  }

  auto findBufferContainingLoc(SourceLoc loc) const -> unsigned {
    assert(loc.isValid() && "Loc should be valid");
    int buffer_id = llvm_src_mgr_.FindBufferContainingLoc(loc.loc_);
    assert(buffer_id != -1 && "Loc should be in a buffer");
    return static_cast<unsigned>(buffer_id);
  }

  /// 给SourceManager增加一个新的MemoryBuffer，持有其所有权。
  auto addNewSourceBuffer(std::unique_ptr<llvm::MemoryBuffer> buffer)
      -> unsigned;

  /// 创建一个MemoryBuffer的拷贝并且添加给SourceManager进行管理，持有其所有权。
  auto addMemBufferCopy(llvm::MemoryBuffer* buffer) -> unsigned;

  /// 创建一个MemoryBuffer的拷贝并且添加给SourceManager进行管理，持有其所有权。
  /// \p input_data 和 \p buf_identifier 为拷贝的数据。
  auto addMemBufferCopy(llvm::StringRef input_data,
                        llvm::StringRef buf_identifier = "") -> unsigned;

  auto getIDForBufferIdentifier(llvm::StringRef buf_identifier)
      -> llvm::Optional<unsigned>;

  auto getLocForBufferStart(unsigned buffer_id) const -> SourceLoc;

  auto getLocOffsetInBuffer(SourceLoc loc, unsigned buffer_id) const
      -> unsigned;

  auto getByteDistance(SourceLoc start, SourceLoc end) const -> unsigned;

  auto getLocForOffset(unsigned buffer_id, unsigned offset) const -> SourceLoc {
    return getLocForBufferStart(buffer_id).getAdvancedLoc(offset);
  }

  auto getLineAndColumn(SourceLoc loc, int buffer_id = -1) const
      -> std::pair<unsigned, unsigned> {
    assert(loc.isValid() && "Loc should be valid");
    return llvm_src_mgr_.getLineAndColumn(loc.loc_, buffer_id);
  }

  auto getRangeForBuffer(unsigned buffer_id) const -> CharSourceRange;

  auto extractText(CharSourceRange range,
                   llvm::Optional<unsigned> buffer_id = llvm::None) const
      -> llvm::StringRef;
};

}  // namespace Radium

#endif  // RADIUM_BASIC_SOURCEMANAGER_H