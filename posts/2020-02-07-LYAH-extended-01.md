-----------
title: LYAH Extended (1) - GHC 类型系统扩展
categories: Haskell
tags: haskell
teaser: Haskell
-----------

# Dependent Types


GHC近几年的一个发展趋势是试图将Dependent Types的概念引入Haskell。什么是Dependent Types？简单的说就是依赖于值的类型(types depend on values)。例如`[a]`(或者说`List a`)在Haskell中是一个列表，它的类型取决于传入的类型(depend on types)而不是传入的值(depend on values)，它可以是`[Int]`，`[Bool]`等等。但当有一些额外的要求时，比如我要定义一个列表类型，这个类型的列表长度大于3(`List n a where n > 3`)，一般来说Haskell是做不到这一点的。很容易想到，这种依赖于值的类型有助于编写更加健壮的程序，很多运行时的bug可以在程序的编译期规避掉，比如如果我有一个保证非零的整数类型，那我可以避免除以零的bug；如果我有非空的列表类型，那可以避免对空列表求`head`导致的Exception等。

<!--more-->

目前为止，Haskell还没有实现完整的对Dependent Types的支持。不过GHC已经做了很多扩展使得部分需求可以实现了。来看一下这些语法扩展：

## ExistentialQuantification
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
printSomething :: forall a. Show a => a -> String
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

[Existential quantification](https://markkarpov.com/post/existential-quantification.html)


## RankNTypes

`forall`关键词除了用于存在量化，还有一个常见的使用方式，就是rank-n-polymorphism，直译的话叫N阶多态。这里的多态和OOP中的多态不同，指对于函数参数类型的多态性(parametric polymorphism)。还是以`id`为例子，一般的带类型变量的多态函数我们写成

``` haskell
id :: a -> a
id x = x
```

如果要定义一个函数，让它接受某个多态函数作为输入呢？

``` haskell
rank2func :: (a -> a) -> (Int, Bool)
rank2func f = (f 0, f True)
```

这里并不能将`id`作为参数传入`rank2func`，虽然看起来类型是匹配的。

``` haskell
test.hs:2:16: error:
    • Couldn't match expected type ‘Int’ with actual type ‘a’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          rank2func :: forall a. (a -> a) -> (Int, Bool)
        at test.hs:1:1-36
    • In the expression: f 0
      In the expression: (f 0, f True)
      In an equation for ‘rank2func’: rank2func f = (f 0, f True)
    • Relevant bindings include
        f :: a -> a (bound at test.hs:2:11)
        rank2func :: (a -> a) -> (Int, Bool) (bound at test.hs:2:1)
  |
2 | rank2func f = (f 0, f True)
  |                ^^^
```

为什么？还是因为Haskell有隐藏的全称量化。如果不做指定，那么类型变量`a`对于`rank2func`函数是全称量化的，并且一旦确定`a`的类型，就无法在`rank2func`函数体中更改。而逻辑上，对`a`的全称量化是传入的参数`f`应该实现的事情，作为consumer我们只考虑怎么利用`f`。这样，量化的约束就放到了`f`中，相当于`f`对`rank2func`隐藏了`a`这个类型变量，可以写成：

``` haskell
rank2func' :: (forall a. a -> a) -> (Int, Bool)     -- 记得启用RankNTypes扩展
rank2func' f = (f 0, f True)
```

最简单的量化了的函数称为一阶多态，依赖于一阶多态的函数就是二阶多态，如上的`rank2func'`。以此类推，Haskell现在可以支持任意阶多态的函数。

经常用于说明`RankNTypes`的例子是[ST Monad](https://en.wikibooks.org/wiki/Haskell/Mutable_objects#The_ST_monad)的函数`runST`。`ST Monad`是Haskell用于实现内部变量的一种Monad，`ST s a`表示在一个状态线程`s`(state thread `s`)中的一系列操作最后产生`a`类型的值。和`ST s a`一起使用的有`STRef s a`，表示在线程`s`中的，有着`a`类型的一个**变量**。可以看一下相关的函数

``` haskell
runST :: forall a. (forall s. ST s a) -> a

newSTRef   :: a -> ST s (STRef s a)
readSTRef  :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s () 
```

从`runST`的定义可以看出它是一个二阶多态的函数，也就是说，从`runST`来看，不用管`s`是什么(它也看不见`s`)，只负责量化`a`就可以了。`runST`保证对于所有类型的`a`，只要给它一个`ST s a`，就能生产出一个`a`。为什么要用到二阶多态？因为逻辑上，不同状态线程之间的`STRef`不能混用。如果没有rank-2-polymorphism，那么调用`runST`的时候我们可以自己指定`ST s1 (STRef s2 a)`，这是不对的。使用rank-2-polymorphism后，`runST`只负责确定`a`，不能指定`s1`或`s2`。下面的声明会报错：

``` haskell
Prelude Control.Monad.ST Data.STRef > v = runST $ newSTRef True

<interactive>:7:13: error:
    • Couldn't match type ‘a’ with ‘STRef s Bool’
        because type variable ‘s’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a type expected by the context:
          forall s. ST s a
        at <interactive>:7:5-25
      Expected type: ST s a
        Actual type: ST s (STRef s Bool)
    • In the second argument of ‘($)’, namely ‘newSTRef True’
      In the expression: runST $ newSTRef True
      In an equation for ‘v’: v = runST $ newSTRef True
    • Relevant bindings include v :: a (bound at <interactive>:7:1)
```

虽然我们可以通过`newSTRef True`拿到一个`ST s (STRef s Bool)`，但因为括号里的`s`和前面的`s`相关，此时`a = STRef s Bool`。`runST`对`a`的全称量化`forall a.`等价于`forall s. STRef s Bool`，而根据前面说的，`runST`无法量化`s`。要正确的使用`runST`，确保它不负责指定状态线程`s`。

``` haskell
{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST
import Data.STRef

mutateVariable = do
  x <- newSTRef (0 :: Int)
  modifySTRef x (+1)
  readSTRef x

y = runST $ mutateVariable

main = print y

-- the output should be
-- 1
```

可以参考：

[Wikibook: Haskell/Polymorphism](https://en.wikibooks.org/wiki/Haskell/Polymorphism)

[Wikibook: Haskell/Mutable objects](https://en.wikibooks.org/wiki/Haskell/Mutable_objects#The_ST_monad)

[Lazy Functional State Thread](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf)

## GADTs
TODO

## DataKinds
TODO

## TypeFamilies
TODO




## 参考

[The Future of Programming is Dependent Types — Programming Word of the Day](https://medium.com/background-thread/the-future-of-programming-is-dependent-types-programming-word-of-the-day-fcd5f2634878)

[Wiki: Dependent type](https://en.wikipedia.org/wiki/Dependent_type)

[Intuitionistic Type Theory](https://plato.stanford.edu/entries/type-theory-intuitionistic/)