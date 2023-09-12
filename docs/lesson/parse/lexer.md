## Trivia 是什么？

在Radium中，"trivia"指的是源代码中的空白、注释和其他不影响语法的元素。例如，空格、换行、制表符和注释都是trivia。理解和跟踪trivia很重要，尤其是当想要保持或操作原始代码格式时。

在Radium的词法分析中，Trivia可以出现在tokens之前或之后。根据它们出现的位置，我们可以将它们分类为：

1. **Leading Trivia**：位于token前面的trivia。
2. **Trailing Trivia**：位于token后面的trivia。

例如，在以下代码片段中：

```swift
// Comment before x
let x = 5 // Comment after 5
```

1. `// Comment before x` 是`let`关键字的leading trivia，因为它们位于该关键字的前面。
2. `// Comment after 5` 以及它前面的空格是数字`5`的trailing trivia，因为它们位于该数字之后。

## Lexer.cpp中的实现

tips:

1. 从`cur_ptr_`获取`tok_start`的时候，总会需要`cur_ptr_ - 1`来获取起始字符，因为在lexer的lexing loop中`switch (*cur_ptr_++)`总会自动将cur_ptr_自增。