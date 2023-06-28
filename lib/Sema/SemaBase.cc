#include "Radium/Sema/SemaBase.h"

#include "Radium/Sema/Sema.h"
#include "llvm/Support/SourceMgr.h"

namespace Radium {

void SemaBase::Note(llvm::SMLoc loc, const char* message) {
  sema_.context_.src_mgr_.PrintMessage(loc, llvm::SourceMgr::DK_Note, message);
}

void SemaBase::Warning(llvm::SMLoc loc, const char* message) {
  sema_.context_.src_mgr_.PrintMessage(loc, llvm::SourceMgr::DK_Warning, message);
}

void SemaBase::Error(llvm::SMLoc loc, const char* message) {
  sema_.context_.src_mgr_.PrintMessage(loc, llvm::SourceMgr::DK_Error, message);
}

}  // namespace Radium