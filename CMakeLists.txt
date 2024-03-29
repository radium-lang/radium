cmake_minimum_required(VERSION 3.6)
project(
  radium
  VERSION 0.0.1
  LANGUAGES C CXX
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_BUILD_TYPE "Debug")
set(project_version "${${PROJECT_NAME}_VERSION}")

message(STATUS "Project '${PROJECT_NAME}', version: '${project_version}'")

option(RADIUM_OPT_BUILD_UNITTESTS "Build all radium unittests" ON)

# temp define: https://discourse.llvm.org/t/python-api-problem/945
add_compile_options(-fno-rtti)

#------------------------------------------------------------
##### LLVM BEGIN #####
# apt install llvm-15-dev libclang-15-dev
set(LLVM_DIR /usr/lib/llvm-15/lib/cmake/llvm)
set(Clang_DIR /usr/lib/llvm-15/lib/cmake/clang)
# manually install
# set(LLVM_DIR /usr/local/lib/cmake/llvm)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
##### LLVM END   #####

#------------------------------------------------------------
# CMake helpers:
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

include(CTest)

include_directories(include)

# Create the RADIUM C++ library
set(radium_SRCS)
set(RADIUM_BASIC_SRCS)
set(RADIUM_PARSE_SRCS)
set(RADIUM_AST_SRCS)

set(STATIC_LIB_NAME ${PROJECT_NAME})
set(RADIUM_BASIC_LIB ${PROJECT_NAME}Basic)
set(RADIUM_PARSE_LIB ${PROJECT_NAME}Parse)
set(RADIUM_AST_LIB ${PROJECT_NAME}AST)

#------------------------------------------------------------
##### radiumBasic lib begin #####
file(GLOB_RECURSE LIB_BASIC_PATH
    ./lib/Basic/*.cc
  )
list(APPEND RADIUM_BASIC_SRCS ${LIB_BASIC_PATH})
add_library(${RADIUM_BASIC_LIB} STATIC ${RADIUM_BASIC_SRCS})
target_link_libraries(${RADIUM_BASIC_LIB}
  LLVMSupport
)
##### radiumBasic lib end   #####

##### radiumParse lib begin #####
file(GLOB_RECURSE LIB_PARSE_PATH
    ./lib/Parse/*.cc
  )
list(APPEND RADIUM_PARSE_SRCS ${LIB_PARSE_PATH})
add_library(${RADIUM_PARSE_LIB} STATIC ${RADIUM_PARSE_SRCS})
target_link_libraries(${RADIUM_PARSE_LIB}
  LLVMSupport
  clangBasic
)
##### radiumParse lib end   #####

# ##### radiumAST lib begin #####
# file(GLOB_RECURSE LIB_AST_PATH
#     ./lib/AST/*.cc
#   )
# list(APPEND RADIUM_AST_SRCS ${LIB_AST_PATH})
# add_library(${RADIUM_AST_LIB} STATIC ${RADIUM_AST_SRCS})
# target_link_libraries(${RADIUM_AST_LIB}
#   LLVMSupport
# )
# ##### radiumAST lib end   #####

#------------------------------------------------------------

# add_subdirectory(tools)

if (RADIUM_OPT_BUILD_UNITTESTS)
  add_subdirectory(unittests #[[EXCLUDE_FROM_ALL]])
endif()
