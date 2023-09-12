#include "Radium/Basic/SourceLoc.h"

#include "Radium/Basic/SourceManager.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

namespace Radium {

auto SourceManager::getCodeCompletionLoc() const -> SourceLoc {
  return getLocForBufferStart(code_completion_buffer_id_)
      .getAdvancedLoc(code_completion_offset_);
}

auto SourceManager::addNewSourceBuffer(
    std::unique_ptr<llvm::MemoryBuffer> buffer) -> unsigned {
  assert(buffer);
  llvm::StringRef buf_identifier = buffer->getBufferIdentifier();
  auto id = llvm_src_mgr_.AddNewSourceBuffer(std::move(buffer), llvm::SMLoc());
  buffer_id_map_[buf_identifier] = id;
  return id;
}

auto SourceManager::addMemBufferCopy(llvm::MemoryBuffer* buffer) -> unsigned {
  return addMemBufferCopy(buffer->getBuffer(), buffer->getBufferIdentifier());
}

auto SourceManager::addMemBufferCopy(llvm::StringRef input_data,
                                     llvm::StringRef buf_identifier)
    -> unsigned {
  auto buffer =
      llvm::MemoryBuffer::getMemBufferCopy(input_data, buf_identifier);
  return addNewSourceBuffer(std::move(buffer));
}

auto SourceManager::getIDForBufferIdentifier(StringRef buf_identifier)
    -> llvm::Optional<unsigned> {
  auto it = buffer_id_map_.find(buf_identifier);
  if (it == buffer_id_map_.end()) {
    return llvm::None;
  }
  return it->second;
}

auto SourceManager::getLocForBufferStart(unsigned buffer_id) const
    -> SourceLoc {
  const auto* buffer = llvm_src_mgr_.getMemoryBuffer(buffer_id);
  return SourceLoc(llvm::SMLoc::getFromPointer(buffer->getBufferStart()));
}

auto SourceManager::getLocOffsetInBuffer(SourceLoc loc,
                                         unsigned buffer_id) const -> unsigned {
  const auto* buffer = llvm_src_mgr_.getMemoryBuffer(buffer_id);
  assert(loc.loc_.getPointer() >= buffer->getBuffer().begin() &&
         loc.loc_.getPointer() <= buffer->getBuffer().end() &&
         "Location is not from the specified buffer");
  return loc.loc_.getPointer() - buffer->getBuffer().begin();
}

auto SourceManager::getByteDistance(SourceLoc start, SourceLoc end) const
    -> unsigned {
#ifndef NDEBUG
  unsigned buffer_id = findBufferContainingLoc(start);
  const auto* buffer = llvm_src_mgr_.getMemoryBuffer(buffer_id);
  assert(end.loc_.getPointer() >= buffer->getBuffer().begin() &&
         end.loc_.getPointer() <= buffer->getBuffer().end() &&
         "End location is not from the same buffer");
#endif
  return end.loc_.getPointer() - start.loc_.getPointer();
}

auto SourceManager::getRangeForBuffer(unsigned buffer_id) const
    -> CharSourceRange {
  const auto* buffer = llvm_src_mgr_.getMemoryBuffer(buffer_id);
  SourceLoc start{llvm::SMLoc::getFromPointer(buffer->getBufferStart())};
  return CharSourceRange(start, buffer->getBufferSize());
}

auto SourceManager::extractText(CharSourceRange range,
                                llvm::Optional<unsigned> buffer_id) const
    -> llvm::StringRef {
  assert(range.isValid() && "range should be valid");

  if (!buffer_id) {
    buffer_id = findBufferContainingLoc(range.getStart());
  }
  llvm::StringRef buffer =
      llvm_src_mgr_.getMemoryBuffer(*buffer_id)->getBuffer();
  return buffer.substr(getLocOffsetInBuffer(range.getStart(), *buffer_id),
                       range.getByteLength());
}

void SourceLoc::printLineAndColumn(llvm::raw_ostream& os,
                                   const SourceManager& src_mgr) const {
  if (isInvalid()) {
    os << "<invalid loc>";
    return;
  }

  auto line_and_column = src_mgr.getLineAndColumn(*this);
  os << "line:" << line_and_column.first << ':' << line_and_column.second;
}

void SourceLoc::print(llvm::raw_ostream& os, const SourceManager& src_mgr,
                      unsigned& last_buffer_id) const {
  if (isInvalid()) {
    os << "<invalid loc>";
    return;
  }

  unsigned buffer_id = src_mgr.findBufferContainingLoc(*this);
  if (buffer_id != last_buffer_id) {
    os << src_mgr->getMemoryBuffer(buffer_id)->getBufferIdentifier();
    last_buffer_id = buffer_id;
  } else {
    os << "line";
  }

  auto line_and_col = src_mgr.getLineAndColumn(*this, buffer_id);
  os << ':' << line_and_col.first << ':' << line_and_col.second;
}

void SourceLoc::dump(const SourceManager& src_mgr) const {
  print(llvm::errs(), src_mgr);
}

void SourceRange::widen(SourceRange other) {
  if (other.start.loc_.getPointer() < start.loc_.getPointer()) {
    start = other.start;
  }
  if (other.end.loc_.getPointer() > end.loc_.getPointer()) {
    end = other.end;
  }
}

auto SourceRange::contains(SourceLoc loc) const -> bool {
  return start.loc_.getPointer() <= loc.loc_.getPointer() &&
         loc.loc_.getPointer() <= end.loc_.getPointer();
}

auto SourceRange::overlaps(SourceRange other) const -> bool {
  return contains(other.start) || other.contains(start);
}

void SourceRange::print(raw_ostream& os, const SourceManager& sm,
                        unsigned& last_buffer_id, bool print_text) const {
  CharSourceRange(sm, start, end).print(os, sm, last_buffer_id, print_text);
}

void SourceRange::dump(const SourceManager& sm) const {
  print(llvm::errs(), sm);
}

CharSourceRange::CharSourceRange(const SourceManager& sm, SourceLoc start,
                                 SourceLoc end)
    : start_(start) {
  assert(start.isValid() == end.isValid() &&
         "Start and end should either both be valid or both be invalid!");
  if (start.isValid()) {
    byte_length_ = sm.getByteDistance(start, end);
  }
}

void CharSourceRange::print(raw_ostream& os, const SourceManager& sm,
                            unsigned& last_buffer_id, bool print_text) const {
  os << '[';
  start_.print(os, sm, last_buffer_id);
  os << " - ";
  getEnd().print(os, sm, last_buffer_id);
  os << ']';

  if (start_.isInvalid() || getEnd().isInvalid()) {
    return;
  }

  if (print_text) {
    os << " RangeText=\"" << sm.extractText(*this) << '"';
  }
}

void CharSourceRange::dump(const SourceManager& sm) const {
  print(llvm::errs(), sm);
}

}  // namespace Radium