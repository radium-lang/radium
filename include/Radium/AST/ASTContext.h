#ifndef RADIUM_AST_ASTCONTEXT_H
#define RADIUM_AST_ASTCONTEXT_H

#include "Radium/AST/ASTAllocated.h"
#include "Radium/Basic/LangOptions.h"
#include "Radium/Basic/SourceManager.h"
#include "llvm/Support/Allocator.h"

namespace Radium {

class ASTContext;

class ConstaintCheckerArenaRAII {
 public:
  ConstaintCheckerArenaRAII(ASTContext& context,
                            llvm::BumpPtrAllocator& allocator);

  ConstaintCheckerArenaRAII(const ConstaintCheckerArenaRAII&) = delete;
  auto operator=(const ConstaintCheckerArenaRAII&)
      -> ConstaintCheckerArenaRAII& = delete;

  ConstaintCheckerArenaRAII(ConstaintCheckerArenaRAII&&) = delete;
  auto operator=(ConstaintCheckerArenaRAII&&)
      -> ConstaintCheckerArenaRAII& = delete;

  ~ConstaintCheckerArenaRAII();

 private:
  ASTContext& self_;
  void* data_;
};

class ASTContext final {
 private:
  ASTContext(LangOptions& lang_opts, SourceManager& sm);

 public:
  struct Implementation;

  auto getImpl() const -> Implementation&;

  void operator delete(void* data) noexcept;

  static auto get(LangOptions& lang_opts, SourceManager& sm) -> ASTContext*;

  ~ASTContext();

  ASTContext(const ASTContext&) = delete;
  void operator=(const ASTContext&) = delete;

  auto Allocate(unsigned long bytes, unsigned alignment,
                AllocationArena arena = AllocationArena::Permanent) const
      -> void* {
    if (bytes == 0) {
      return nullptr;
    }

    if (lang_opts.UseMalloc) {
      return AlignedAlloc(bytes, alignment);
    }

    if (arena == AllocationArena::Permanent && stats) {
      //stats->getFro
    }
  }

 public:
  const LangOptions& lang_opts;

  SourceManager& src_mgr;

  friend class ConstaintCheckerArenaRAII;
};

}  // namespace Radium

#endif  // RADIUM_AST_ASTCONTEXT_H