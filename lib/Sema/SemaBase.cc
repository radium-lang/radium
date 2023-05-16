#include "radium/Sema/SemaBase.h"

#include "llvm/Support/SourceMgr.h"
#include "radium/Sema/Sema.h"

using namespace Radium;

void SemaBase::Note(llvm::SMLoc Loc, const char* Message) {
  S.Context.SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Note, Message);
}

void SemaBase::Warning(llvm::SMLoc Loc, const char* Message) {
  S.Context.SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Warning, Message);
}

void SemaBase::Error(llvm::SMLoc Loc, const char* Message) {
  S.Context.SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Error, Message);
}