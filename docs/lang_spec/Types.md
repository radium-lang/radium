# 类型

在Radium中，有两种类型：

- 命名类型（Named Type）：即在定义类型的时候具备实际的命名。比如用户定义类（class）叫**MyClass**，则该类具备类型（**MyClass**）。其他语言中的基本的数据类型，在Radium中都为命名类型。正因为它们是命名类型，则可以很方便针对于类型做一些扩展行为。
- 复合类型（Compound Type）：复合类型没有名字。Radium中存在两种符合类型：**函数类型**和**元组（tuple）类型**，例如tuple类型如`(Int, (Int, Int))`,

Swift类型的语法：
```haskell
type → function-type

type → array-type

type → dictionary-type

type → type-identifier

type → tuple-type

type → optional-type

type → implicitly-unwrapped-optional-type

type → protocol-composition-type

type → boxed-protocol-type

type → opaque-type

type → metatype-type

type → any-type

type → self-type

type → ( type )
```

Radium类型的语法：
```haskell
type → function-type
type → type-identifier
type → metatype-type
type → self-type
type → ( type )
```
