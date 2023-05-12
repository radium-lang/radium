#ifndef RADIUM_SEMA_SEMA_H
#define RADIUM_SEMA_SEMA_H

namespace Radium {

class Sema {
 public:
  Sema() = default;
  ~Sema() = default;

  Sema(const Sema&) = delete;
  void operator=(const Sema&) = delete;
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMA_H