---
layout: article
categories: Technology
tags: haskell Language
title: Haskell 自学笔记1：Functor, Applicative Functor, Monoid, Monad
---

光看书，这些概念实在是有点绕，借用GitHub Pages整理归纳一下。初学Haskell，还没怎么正式用过，理解上的偏差无可避免。如果有任何错误或遗漏，在后续的博客中改正。

以下内容主要参考 _Learn you a Haskell for great good_[^fn1].

## 什么是Functor（函子）？

Functor指的是一个容器所具有的某种性质：这个容器实现了一个称为fmap的函数，可以将某个单参数函数提升为对容器中的元素操作的函数。即：把一个函数应用到容器内部。

如果说容器是一个上下文环境，那么fmap使得你可以将一个上下文无关的函数应用到这个容器，也即：fmap是函数的上下文相关版本。

```haskell
class Functor f where
	fmap :: (a->b) -> f a -> f b
```

### 从范畴论到Functor

[范畴论的数学基础](https://en.wikibooks.org/wiki/Haskell/Category_theory)

范畴的三个组成部分

1. 对象
2. 态射，用以连接范畴中的两个对象
3. 态射的复合，用以将多个态射组合到一起

范畴的三个定律

1. 态射需要满足结合律，即：`f.(g.h) = (f.g).h`
2. 范畴对于态射的复合来说是封闭的，即：若 `f`, `g` 属于某范畴，则`f.g`也属于该范畴
3. 对每个范畴中的对象，都存在一个恒等的态射

### Haskell的范畴：Hask

Haskell的所有类型types，对应Hask的objects。Haskell的所有函数functions，对应Hask的morphisms

范畴论中的函子F是范畴到范畴的变换(transformation)。给定函子 `F: C -> D`，`F` 应该：
1. 将C中的所有object映射到 `D：A -> F(A)`，这个在Hask中对应类型构造器
2. C中的所有态射也映射到 `D：f -> F(f)`，在Hask中对应高阶函数

### 函子的定律

函子应该满足两条定律：
1. 范畴`C`中，任意对象`A`上的恒等变换`id(A)`，可以通过F变换为范畴`D`中的恒等变换，即 `F(id(A)) = id(F(A))`
2. 函子对于态射的复合应该满足分配律，即：`F(f.g) = F(f).F(g)`

Haskell中的函子实际上是从范畴Hask到范畴func的一个变换，其中func是通过函子所定义的一个Hask的子范畴。
对象到对象的映射已经通过函子f完成了，而态射到态射的映射则是通过函子f的一个接口：即前面提到的fmap

范畴论中函子的两条定律，对Haskell中的函子依然适用：
1. 恒等变换：`fmap id = id`
2. 分配律：`fmap (f.g) = fmap f.fmap g`



## 什么是Applicative Functor（应用型函子）？

```haskell
class (Functor f) => Applicative f where
	pure a :: f a
	(<*>) :: f (a -> b) -> f a -> f b
```

对于范畴`C`内的函数`(a -> b)` ，我们有函子可以将它应用于范畴`D`。但如果我们只有范畴`D`内的函数`f(a -> b)`呢？能不能将它也应用于范畴`D`呢？

Applicative Functor指的是这样一个容器：它除了具有Functor类型容器的性质外，你还可以
1. 将任意类型用该容器封装。记住functor实际上是个类型的构造器，接受类型作为参数。
2. 对于一个容器内部的函数，你可以将它拿出来并应用于容器内的元素

所以对于Applicative Functor函子，你既可以将外部的普通函数提升为应用于容器元素的函数（满足函子的定义），也可以将容器内部存在的函数拿出来，将其应用于容器元素。另外，pure函数的存在让你可以将任意类型放入容器中，这个任意类型当然也包括函数类型。

Applicative Functor有用的地方在于，它可以存放类型，也可以存放类型到类型的映射。它的应用范围比functor大了很多。

考虑所有的a, b, 以及(a -> b), 它们组成一个范畴`C`。所有的`f a`, `f b`, 以及`f (a -> b)`，它们也组成一个范畴 `C'`。而`Applicative Functor f`就是范畴 `C` 到 `C'` 的映射。

### Applicative函子的定律

就像函子一样，Applicative函子也有需要满足的定律，它们是

```haskell
pure id <*> v = v                            -- 恒等
pure f <*> pure x = pure (f x)               -- 同态
u <*> pure y = pure ($ y) <*> u              -- 交换
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- 复合
```

另外，还有一条关于 `fmap`的性质：

```haskell
fmap f x = pure f <*> x                      -- fmap
```

### Applicative函子的用途

因为没什么深刻的体会，这里具体的用途只能留待日后补全。书中主要讲了两个使用到应用型函子的地方：`liftA2`和`sequenceA`。

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

`liftA2`让我们可以将两个应用型函子作为容器展开，返回一个更大的容器，里面包含了对展开的元素元组应用`f`的结果。而`sequenceA`则将一个应用型函子的列表展开，返回一个用应用型函子包起来的列表。

## 什么是Monad（单子）？

Monad是一类特殊的函子，是范畴到其自身的映射，同时对于范畴中的每一个对象，它都定义了两个态射：

```haskell
unit(x) :: x -> M(x)
join(x) :: M(M(x)) -> M(x)
```

这里，`unit(x)`可以认为是x从范畴C到范畴D的最小上下文映射，而`join(x)`则是将范畴D中的对象展平。

Haskell中的单子一般通过下面的方式定义：
```haskell
class (Functor m) => Monad m where
	return :: x -> m x
	x >>= f :: m a -> (a -> m b) -> m b
```

虽然形式不同，但可以通过一些变换证明，join和绑定>>=是等价的

```haskell
join x = x >>= id
```

```haskell
x >>= f = join (fmap f x)
```

### Monad需要满足的定律

Haskell中Monads需要满足如下定律

```haskell
m >>= return     =  m                        --右单位元
return x >>= f   =  f x                      --左单位元
(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  --结合律
```

## 什么是Monoid（幺半群）？

```haskell
class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

```
`Monoid` 是一个包含二元操作符和单位元的类型类，其中二元操作符mappend需要满足结合律。

为什么要搞这么个概念呢？暂时还无法理解，只能安慰自己说Haskell习惯把能抽象的概念都抽象了……

书中一个比较nb的例子是`Ordering`，它也是个 `Monoid` ！

```haskell
data Ordering = LT | EQ | GT

instance Monoid Ordering where
	mempty = EQ
	LT `mappend` _ = LT
	EQ `mappend` y = y
	GT `mappend` _ = GT
```
`Ordering`是 `Monoid` 的事实实际上体现了一个分优先级的比较。要比较a和b，先比较a1和b1。a和b的大小首先取决于a1 b1，以此类推。

## 一些问题待以后解决

`(>>=)` 为什么接受 `(a -> m b)` 而不是 `(m a -> m b)`？

applcative和monad的区别？

## 总结

写完这篇博客，还是感觉自己对这几个概念一知半解。虽然很想在理解的更透彻一些后总结，但暂时就此打住吧。期待以后有了更多的实践经验后再来补充。


## 参考文献
[^fn1]: [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)

[Haskell/Category theory](https://en.wikibooks.org/wiki/Haskell/Category_theory)

[Monads As Containers](https://wiki.haskell.org/Monads_as_containers)

[Monads for Curious Programmers](https://bartoszmilewski.com/2011/01/09/monads-for-the-curious-programmer-part-1/)

[Applicative programming with effects](http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf)
