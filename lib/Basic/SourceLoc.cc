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
  auto id = llvm_src_mgr_.AddNewSourceBuffer(
      std::move(buffer), llvm::SMLoc());
  buffer_id_map_[buf_identifier] = id;
  return id;
}

auto SourceManager::addMemBufferCopy(llvm::MemoryBuffer* buffer) -> unsigned {
  return addMemBufferCopy(buffer->getBuffer(), buffer->getBufferIdentifier());
}

auto SourceManager::addMemBufferCopy(llvm::StringRef input_data,
                                     llvm::StringRef buf_identifier)
    -> unsigned {
  auto buffer = llvm::MemoryBuffer::getMemBufferCopy(input_data, buf_identifier);
  return addNewSourceBuffer(std::move(buffer));
}

auto SourceManager::getIDForBufferIdentifier(StringRef buf_identifier)
    -> Optional<unsigned> {
  auto it = buffer_id_map_.find(buf_identifier);
  if (it == buffer_id_map_.end()) {
    return Nothing;
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

void SourceRange::print(raw_ostream& os, const SourceManager& sm,
                        unsigned& last_buffer_id, bool print_text) const {
  os << '[';
  start.print(os, sm, last_buffer_id);
  os << " - ";
  end.print(os, sm, last_buffer_id);
  os << ']';

  if (start.isInvalid() || end.isInvalid()) {
    return;
  }

  if (print_text) {
    os << " RangeText=\""
       << StringRef(start.loc_.getPointer(),
                    end.loc_.getPointer() - start.loc_.getPointer() + 1)
       << '"';
  }
}

void SourceRange::dump(const SourceManager& sm) const {
  print(llvm::errs(), sm);
}

}  // namespace Radium