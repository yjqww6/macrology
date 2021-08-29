# macrology

这个仓库的主题是Racket的宏系统，主要目的是收集并记录编写宏过程中一些常见的技巧和模式。

Racket社区中有大量关于宏的知识技巧处于口口相传或是过于碎片化的状态，并没有很好地写成文字和索引。

这里不提供入门教程，相关资料，Racket的或是Scheme的，已经有很多了：

* [Racket Guide - Macros](https://docs.racket-lang.org/guide/macros.html)
* [Fear of Macros](http://www.greghendershott.com/fear-of-macros/)
* <https://docs.racket-lang.org/syntax/stxparse-intro.html>
* [Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/)的1-3节


## 目录

### 一些用法

* TODO Racket宏特性概述
* [如何让DrRacket正确地画出箭头](https://github.com/yjqww6/macrology/blob/master/draw-arrow.md)
* [local-expand该怎么用](https://github.com/yjqww6/macrology/blob/master/local-expand.md)
* [如何设置编译期信息](https://github.com/yjqww6/macrology/blob/master/compenv.md)
* [如何使用First Class Internal Definition Context](https://github.com/yjqww6/macrology/blob/master/intdef-ctx.md)
* [如何使用First Class Internal Definition Context(旧)](https://github.com/yjqww6/macrology/blob/master/intdef-ctx-old.md)


### 疑难分析

* [Phase 与 identifier的匹配](https://github.com/yjqww6/macrology/blob/master/phase-match.md)
* [Scope和Binding](https://github.com/yjqww6/macrology/blob/master/scope.md)
* [可扩展的宏](https://github.com/yjqww6/macrology/blob/master/Extensible%20Macros.md)



## 其他资料

* [Keeping it Clean with Syntax Parameters](http://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf)
* [Debugging Hygienic Macros](https://www2.ccs.neu.edu/racket/pubs/cf-sp09.pdf)
* [Fortifying macros](https://www2.ccs.neu.edu/racket/pubs/c-jfp12.pdf)
* [Advanced Macrology and the Implementation of Typed Scheme](https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf)
