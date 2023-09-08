#ifndef RADIUM_BASIC_FALLTHROUGH_H
#define RADIUM_BASIC_FALLTHROUGH_H

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

#if __has_attribute(fallthrough)
#define RADIUM_FALLTHROUGH [[clang::fallthrough]]
#else
#define RADIUM_FALLTHROUGH
#endif

#endif  // RADIUM_BASIC_FALLTHROUGH_H