-----------
title: LYAH Extended (1) - GHC 类型系统扩展
categories: Haskell
tags: haskell
teaser: Haskell
-----------

# Dependent Types


GHC近几年的一个发展趋势是试图将Dependent Types的概念引入Haskell。什么是Dependent Types？简单的说就是依赖于值的类型(types depend on values)。例如`[a]`(或者说`List a`)在Haskell中是一个列表，它的类型取决于传入的类型(depend on types)而不是传入的值(depend on values)，它可以是`[Int]`，`[Bool]`等等。但当有一些额外的要求时，比如我要定义一个列表类型，这个类型的列表长度大于3(`List n a where n > 3`)，一般来说Haskell是做不到这一点的。很容易想到，这种依赖于值的类型有助于编写更加健壮的程序，很多运行时的bug可以在程序的编译期规避掉，比如如果我有一个保证非0的整数类型，那我可以避免除以0的bug；如果我有非空的列表类型，那可以避免对空列表求`head`导致的Exception等。

目前为止，Haskell还没有实现完整的对Dependent Types的支持。不过GHC已经做了很多扩展使得部分需求可以实现了。来看一下这些语法扩展：

## `ExistentialQuantification`
Existential Quantification翻译为存在量化。这个扩展和Dependent Types没啥关系，不过可以放在一起说一下。

对于类型变量，如果不做其他声明，Haskell默认有一个隐藏的全称量化(Universal Quantification)。例如

``` haskell
id :: a -> a
```

实际上等价于

``` haskell
id :: forall a. a -> a
```

也就是说，`a`是个类型变量，`id`对于作为其参数的**所有类型**都要能返回同样的类型。这个称为**全称量化**。容易想到这样的函数实际上只能写为`id x = x`。那如果想写出如下类型的函数呢？

``` haskell
printSomething :: a -> String
```

很难写出一个有实际意义的函数来(`const`这种不考虑)，因为`a`默认是全称量化的，必须要考虑到所有的可能性。但是如果加一些限制进去，例如下面

``` haskell
printSomething :: forall a. Show a => a -> a
```

那我们马上可以写出`printSomething = show`。这里的`forall a. Show a`就是Haskell的存在量词，表示存在一些类型`a`(是`Show`的instance)能够提供给`printSomething`。这样，`printSomething`就被**存在量化**了。[出于一些原因](https://prime.haskell.org/wiki/ExistentialQuantification)，Haskell并没有采用`exists a.`作为存在量词。

还有一种情况是在写类型的构造器时，存在量化允许我们在构造器中引入非参数的类型变量，也就是不出现在等号左边的变量。

如下面的例子：

``` haskell
Prelude > data Foo a = MkFoo a              -- OK
Prelude > data Foo1 = MkFoo1 a              -- 类型变量a必须作为该类型定义的一个参数引入
<interactive>:8:20: error: Not in scope: type variable ‘a’ 

Prelude > :set -XExistentialQuantification
Prelude > data Foo2 = forall a. MkFoo2 a    -- OK
```

别忘了类型构造器实际上就是函数。因此我们有

``` haskell
MkFoo2 :: forall a. a -> Foo2               -- 存在某些a，可以用来构造Foo2
```

这么写，用起来就麻烦了，因为不知道`a`是什么。所以要使用存在量化一般会跟上constraints，或者利用构造器内提供的各种函数。


``` haskell
fFoo2 :: Foo2 -> Bool
fFoo2 (MkFoo2 x) = ???                          -- 不知道x的类型，无法使用

data Foo3 = forall a. MkFoo3 a (a -> Bool)

fFoo3 :: Foo3 -> Bool
fFoo3 (MkFoo3 x f) = f x                        -- 通过构造器提供的函数f来使用x
```

如下的两种定义是等价的（当然后一种在Haskell中并不存在）：

``` haskell
data Foo = forall a. MkFoo a (a -> Bool)

data Foo = MkFoo (exists a . (a, a -> Bool))
```

可以参考：

[What does the `forall` keyword in Haskell/GHC do?](https://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do)

## `RankNTypes`
TODO

## `GADTs`
TODO

## `TypeFamilies`
TODO




## 参考

[The Future of Programming is Dependent Types — Programming Word of the Day](https://medium.com/background-thread/the-future-of-programming-is-dependent-types-programming-word-of-the-day-fcd5f2634878)

[Wiki: Dependent type](https://en.wikipedia.org/wiki/Dependent_type)

[Intuitionistic Type Theory](https://plato.stanford.edu/entries/type-theory-intuitionistic/)