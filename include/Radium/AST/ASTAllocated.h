#ifndef RADIUM_AST_ASTALLOCATED_H
#define RADIUM_AST_ASTALLOCATED_H

#include <cassert>
#include <cstddef>

namespace Radium {

class ASTContext;

enum class AllocationArena { Permanent, ConstraintSolver };

namespace detail {
auto allocateInASTContext(size_t bytes, const ASTContext& ctx,
                          AllocationArena arena, unsigned alignment) -> void*;
}

template <typename AlignTy>
class ASTAllocated {
 public:
  auto operator new(size_t bytes) noexcept -> void* = delete;
  void operator delete(void* data) noexcept = delete;

  auto operator new(size_t bytes, const ASTContext& ctx,
                    AllocationArena arena = AllocationArena::Permanent,
                    unsigned alignment = alignof(AlignTy)) -> void* {
    return detail::allocateInASTContext(bytes, ctx, arena, alignment);
  }

  auto operator new(size_t bytes, void* mem) noexcept -> void* {
    assert(mem && "placement new into failed allocation");
    return mem;
  }
};

}  // namespace Radium

#endif  // RADIUM_AST_ASTALLOCATED_H