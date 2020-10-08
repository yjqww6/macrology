# macrology

这个仓库的主题是Racket的宏系统，主要目的是收集并记录编写宏过程中一些常见的技巧和模式。

Racket社区中有大量关于宏的知识技巧处于口口相传或是过于碎片化的状态，并没有很好地写成文字和索引。

这里不提供入门教程，相关资料，Racket的或是Scheme的，已经有很多了：

* [Racket Guide - Macros](https://docs.racket-lang.org/guide/macros.html)
* [Fear of Macros](http://www.greghendershott.com/fear-of-macros/)



## 目录

### 一些用法

* [local-expand该怎么用](https://github.com/yjqww6/macrology/blob/master/local-expand.md)
* TODO Syntax Property有什么应用
* TODO The Racket Reference中Syntax Transformers一节提到的特性都有什么用

* [如何使用First Class Internal Definition Context](https://github.com/yjqww6/macrology/blob/master/intdef-ctx.md)
* TODO 如何遍历Fully Expanded Program
* [如何让DrRacket正确地画出箭头](https://github.com/yjqww6/macrology/blob/master/draw-arrow.md)
* TODO Interposition Points的应用

### 疑难分析

* [可扩展的宏](https://github.com/yjqww6/macrology/blob/master/Extensible%20Macros.md)



## 其他资料

* [Keeping it Clean with Syntax Parameters](http://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf)
* [Debugging Hygienic Macros](https://www2.ccs.neu.edu/racket/pubs/cf-sp09.pdf)
* [Fortifying macros](https://www2.ccs.neu.edu/racket/pubs/c-jfp12.pdf)

* [Advanced Macrology and the Implementation of Typed Scheme](https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf)
