-----------
title: LYAH Extended (0) - 引子
categories: Haskell
tags: haskell
teaser: Haskell
-----------

[Learn You A Haskell For Great Good](http://learnyouahaskell.com/) 是一本很好的Haskell语言入门书籍，但它可能也是唯一一本让我读完以后写不出一个“像样”的Haskell程序的入门书。就像前一段时间 [Reddit上讨论的](https://www.reddit.com/r/haskell/comments/ej2g06/the_simple_haskell_initiative/)，Simple Haskell和Fancy Haskell的区别。在Reddit泡的时间越长，越发觉得除我之外的所有人都在写Fancy Haskell。

我把一部分原因归于GHC的发展。作为事实上的Haskell编译器，GHC一直在不断引入新特性，使得它实际支持的语法比 [Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/) 定义要大得多。翻开GHC的用户手册，第九章 [GHC Language Features](https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/lang.html)，里面的小节从9.1一直排到9.40，可怕。当然并不是所有的GHC扩展都同等重要，但某些语法扩展确实比其他的更重要。

因此我想简单写一些关于GHC/Reddit常用或常提及，但又不在LYAH里面被包括的语法和范式，我称之为LYAH Extended。大致分为三块：一是GHC对Haskell类型系统的扩展，二是和应用范畴论相关的一些库和功能，三是Effect System。题目很大，我也知道凭目前的经验我写不好，但我还是想去做。

<!--more-->