#ifndef RADIUM_AST_PRINTOPTIONS_H
#define RADIUM_AST_PRINTOPTIONS_H

namespace Radium {

struct PrintOptions {
  // 标志符的宽度。
  unsigned indent = 2;

  // 是否打印函数定义。
  bool function_definitions = false;

  // 是否打印类型定义。
  bool type_definitions = false;

  // 是否打印变量初始化器。
  bool variable_initializers = false;

  // 是否打印AST的隐式部分。
  bool skip_implicit = false;

  static auto printEverything() -> PrintOptions {
    PrintOptions result;
    result.function_definitions = true;
    result.type_definitions = true;
    result.variable_initializers = true;
    result.skip_implicit = false;
    return result;
  }
};

}  // namespace Radium

#endif  // RADIUM_AST_PRINTOPTIONS_H