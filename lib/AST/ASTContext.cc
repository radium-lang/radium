#include "Radium/AST/ASTContext.h"

#include "llvm/ADT/FoldingSet.h"

namespace Radium {

struct ASTContext::Implementation {
  Implementation();
  ~Implementation();

  struct Arena {
    llvm::FoldingSet<TupleType> tuple_types;

    ~Arena();
  };

  struct ConstaintSolverArena : public Arena {
    ConstaintSolverArena(llvm::BumpPtrAllocator& allocator)
        : allocator(allocator) {}

    ConstaintSolverArena(const ConstaintSolverArena&) = delete;
    auto operator=(const ConstaintSolverArena&)
        -> ConstaintSolverArena& = delete;
    ConstaintSolverArena(ConstaintSolverArena&&) = delete;
    auto operator=(ConstaintSolverArena&&) -> ConstaintSolverArena& = delete;

    llvm::BumpPtrAllocator& allocator;
  };

  llvm::BumpPtrAllocator* allocator;
  std::vector<std::function<void(void)>> cleanups;
};

ConstaintCheckerArenaRAII::ConstaintCheckerArenaRAII(
    ASTContext& context, llvm::BumpPtrAllocator& allocator)
    : self_(context),
      data_(self_.getImpl().CurrentConstraintSolverArena.release()) {
  self_.getImpl().CurrentConstraintSolverArena.reset(
      new ASTContext::Implementation::ConstaintCheckerArena(allocator));
}

ConstaintCheckerArenaRAII::~ConstaintCheckerArenaRAII() {
  self_.getImpl().CurrentConstraintSolverArena.reset(
      static_cast<ASTContext::Implementation::ConstaintCheckerArena*>(data_));
}

}  // namespace Radium