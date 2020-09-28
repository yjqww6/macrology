# 可扩展的宏

（早期版本：<https://zhuanlan.zhihu.com/p/35953489> <https://zhuanlan.zhihu.com/p/36709037> )

一些宏中会提供扩展机制，例如 `require` 、`provide` 、`for` 、`match` 以及 `syntax-parse`。

其特征是需要用户自行定义一个宏A，并把A的名字提供给这些宏。A的过程会被调用，结果会被用于下一步的操作。

一个例子：

```Racket
#lang racket
(define-match-expander succ
  (λ (stx)
    (syntax-case stx ()
      [(_ ?N)
       #'(app sub1 ?N)])))

(match 5
  [(succ (succ N)) N])
```

这里用户定义了succ（为什么会用 `define-match-expander` 而不是 `define-synax` 后面会说），对应的过程被match调用，最后匹配模式变成了 `(app sub1 (app sub1 N))` 。最终输出3。



## 天真的实现

因为宏绑定的过程可以用 `syntax-local-value` 获得，所以按直觉写出来的代码一般类似：

```racket
#lang racket

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     ((syntax-local-value #'id) #'in)]))

(use-expander expander1 car)
```

这里直接调用expander1的过程，而不是将其用于宏展开，最后展开为 `(car x)` ，结果得到1。

### 毛病

Racket的宏是卫生的，不是同一次的宏展开引入的名字默认不会意外绑定到彼此。然而，上面的方案中，expander1的调用仍处于use-expander的展开中，因此expander1引入的x是有可能出问题的。

```racket
#lang racket

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     #`(let ([x '(2)])
         #,((syntax-local-value #'id) #'in))]))

(use-expander expander1 car)
```

这里x绑定到了use-expander局部引入的x，最终运行得到2。

## 早期的解决方案

Racket社区早期的解决方案是手动模拟宏展开过程中的macro-introduction scopes的双重反转。至于Racket宏的卫生原理文档里早有论述，这里不多做说明，见：

* <https://www.cs.utah.edu/plt/scope-sets/>
* <https://docs.racket-lang.org/reference/syntax-model.html#%28part._transformer-model%29>
* <https://www.youtube.com/watch?v=Or_yKiI3Ha4>

```racket
#lang racket

(begin-for-syntax
  (define (apply-expander proc stx)
    (define introducer (make-syntax-introducer))
    (define intro-stx (introducer (syntax-local-introduce stx)))
    (syntax-local-introduce (introducer (proc intro-stx)))))

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'in))]))

(use-expander expander1 car)
```

这个方法的核心在于apply-expander过程：

1. 输入的stx首先被应用syntax-local-introduce，模拟use-expander的展开过程暂时结束，准备进入新一次的宏展开。
2. introducer被应用，模拟进入了新一次的宏展开，引入新的macro-introduction scope。
3. proc被应用，得到expander1的结果。
4. introducer再次被应用，模拟expander1的展开结束，反转此次的macro-introduction scope。
5. syntax-local-introduce再次被应用，回到use-expander的展开过程中。

这样，use-expander1的x被打上了唯一的macro-introduction scope，expander1的x不可能具有该scope，不会意外的绑定。

值得注意的是，这里syntax-local-introduce和introducer都是必要的：

* 缺少syntax-local-introduce的话，expander1的x会带上use-expander的macro-introduction scopr，仍会绑定到use-expander的x。
* 缺少introducer的话，如果use-expander不止能调用expander1，还能调用更多的宏，例如expander2，那么expander1和expander2的结果可能意外地互相绑定。另外如果expander1引入定义，use-expander引入使用的话，也可能会出问题。

## 插曲：local-apply-transformer

在7.0版本（准确来说，6.90.0.29）中，Racket正式引入了local-apply-transformer这个api，它解决了什么问题呢？

如果上面的expander1也想要自己支持扩展功能，它能复用apply-expander过程吗？不能，因为syntax-local-introduce是属于use-expander的，introduer才是expander1自己的"syntax-local-introduce"。注意到上面的introducer是apply-expander的一个局部变量，这样expander1甚至无法访问到自己的introducer，自然也不能用来写 扩展了。

传统的解法是把introducer绑定到一个parameter里面，这样expander1就可以访问它了。这就是为什么会有 `syntax-local-match-introduce` , `syntax-local-require-introduce` , `syntax-local-provide-introduce` ,   `syntax-local-syntax-parse-pattern-introduce` , ……

但是这样expander1怎么知道以自己的身份到底该用哪个introducer呢？也许大家再共用一个"current-introduce"？这样就太容易遗漏了。

对于这个问题，local-apply-transformer的方案是不直接调用expander1的过程，而是通过local-expand将其作为一次宏展开间接地调用。这样，expander1就是一次普通的宏展开了，对应的introducer就是普通的syntax-local-introduce，上面那堆xxx-introduce也就不需要了。

因此，上面的写法演变为

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx 'expression)))

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'in))]))

(use-expander expander1 car)
```

## 还有问题？

回到早期方案，来看看这个程序：

```racket
#lang racket
(begin-for-syntax
  (define (apply-expander proc stx)
    (define introducer (make-syntax-introducer))
    (define intro-stx  (introducer (syntax-local-introduce stx)))
    (syntax-local-introduce (introducer (proc intro-stx)))))

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(an-x id) #'(let ([an-x '(3)])
                   (id x))]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'(an-x in)))]))

(use-expander x expander1 car)
```

得到了3，为什么呢？

简单来说，在apply-expander时，因为use-expander的定义和使用在同一definition context，syntax-local-introduce反转的不仅有macro-introduction scope，还有use-site scope，因此expander1中的x被打上了use-site scope，然后use-site scope在宏展开结束时不会被反转。an-x和x都带有该scope，导致返回3。

但是对一个卫生的宏系统，这里应该得到1，就像直接调用 `(expand1 x car)` 那样。

我们无法直接模拟use-site scope，所以直接在早期方案上改进似乎无法解决这个问题。（注：一些失败的尝试可在<https://zhuanlan.zhihu.com/p/36709037>看到）

## local-apply-transformer可以解决问题吗？

将上面的apply-expander定义替换，得到（记为程序A）

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx 'expression)))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'(an-x in)))]))
  
(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(an-x id) #'(let ([an-x '(3)])
                   (id x))]))

(use-expander x expander1 car)
```

这次是1了，那么问题解决了吗？

并没有，把expander1的定义和使用套进let里（记为程序B），仍得到3

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx 'expression)))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'(an-x in)))]))
  
(define x '(1))

(let ()
  (define-syntax (expander1 stx)
    (syntax-case stx ()
      [(an-x id) #'(let ([an-x '(3)])
                     (id x))]))

  (use-expander x expander1 car))
```

为什么在A里是1？因为用local-apply-transformer的话，结果的use-site scope就跟普通的宏展开一样，不会被反转。这样，expander1里(id x)的x没有use-site scope。这就是它跟早期方案的不同。

那为什么在B里又变回3了？因为use-expander的定义和使用不在同一definition context了，所以也就都没有use-site scope了。

可以认为上面对local-apply-transformer的使用效果类似于

```racket
(begin-for-syntax
  ;;只反转macro-introduction scope
  (define (macro-scope-introducer)
    (make-syntax-delta-introducer
     (syntax-local-identifier-as-binding
      (syntax-local-introduce #'x))
     #'x))
  (define (apply-expander proc stx)
    (define macro-introducer (macro-scope-introducer))
    (define introducer (make-syntax-introducer))
    (define intro-stx  (introducer (macro-introducer stx)))
    (macro-introducer (introducer (proc intro-stx)))))
```

实际上，在此文写下的时间点，Racket标准库里也有这个问题：

```racket
#lang racket
(define x sub1)
(define-match-expander succ
  (lambda (stx)
    (syntax-case stx ()
      [(_ v)  #'(app (let ([v add1]) x) v)])))

(match 3
  [(succ x) x]) ;得到4，而不是2
```

### 到底错在哪里？

仔细观察会发现，use-expander的use-site scope并不是问题所在。

不妨先看看普通宏版本是什么情况：

```racket
#lang racket

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #'(let ([x '(2)])
         (id an-x in))]))
  
(define x '(1))
(let ()
  (define-syntax (expander1 stx)
    (syntax-case stx ()
      [(_ an-x id) #'(let ([an-x '(3)])
                       (id x))]))

  (use-expander x expander1 car))
```

这里use-expander先被展开，因此 `(let ([x '(2)]) ...)` 这里带有了let自己的local scope。同时，因为`(let ([an-x '(3)]) ...)` 这里的an-x是use-expander传来的，这样两个局部的x都带有该local scope。然而，(id x)的x是expander1新引入的，没有该local scope，所以只能绑定到module顶部的x。

另一方面，如果不要use-expander内部的let：

```racket
#lang racket

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #'(id an-x in)]))
  
(define x '(1))
(let ()
  (define-syntax (expander1 stx)
    (syntax-case stx ()
      [(_ an-x id) #'(let ([an-x '(3)])
                       (id x))]))

  (use-expander x expander1 car))
```

这里则是expander1自身定义和使用在同一definition context，an-x带有use-site scope，因此也能得到1。

所以真正的问题有两个：

1. 内部的expander1先于外部的use-expander被展开，以至内部被打上了本不该有的local scope。
2. expander1的展开没有适当地引入use-site scope。事实上，在expression context下，local-apply-transformer（或者说local-expand）永远不会引入use-site scope，这是与Racket 7之前的expander的一个差异。

这两个问题归根到底都是local-expand的问题，似乎可能通过把宏写成cps形式回避，但相信没人会愿意这么写吧。

另一方面，如果把local-apply-transformer从expression context换到internal definition context，则会无条件引入use-site scope。

确实，如果这么修改，A和B都能得到1。

但这并不意味着完全对了。看程序C：

```racket
#lang racket
(require (for-syntax syntax/apply-transformer syntax/context))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx (generate-expand-context))))

(define x '(1))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'(an-x))
         (in an-x))]))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(an-x) #'(define an-x '(3))]))

(use-expander x expander1 car)
```

这里还是得到1。然而对于普通宏，

```racket
#lang racket

(define x '(1))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         (id an-x)
         (in an-x))]))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(_ an-x) #'(define an-x '(3))]))

(use-expander x expander1 car)
```

这次则是3了。

可以认为上面对local-apply-transformer的使用效果类似于
```racket
(begin-for-syntax
  (define (macro-scope-introducer)
    (make-syntax-delta-introducer
     (syntax-local-identifier-as-binding
      (syntax-local-introduce #'x))
     #'x))
  (define (apply-expander proc stx)
    (define macro-introducer (macro-scope-introducer))
    (define introducer (make-syntax-introducer))
    (define use-site-introducer (make-syntax-introducer #t))
    (define intro-stx  (use-site-introducer (introducer (macro-introducer stx))))
    (macro-introducer (introducer (proc intro-stx)))))
```

## 结论

对于这个问题，现在仍没有完美的解决方案。

如果不考虑C那样展开为define的情况，可能 `(local-apply-transformer proc stx (generate-expand-context))` 是比较合适的解法。

最终的解决方案可能类似于 <https://github.com/racket/racket/pull/2237> ，但似乎凉了。

## 其他相关问题

### 不要使用syntax-rules
编写expander1的时候，注意不要使用syntax-rules，因为其会对结果应用syntax-protect。而一般写use-expander这类宏的时候，是不会特意去syntax-disarm的，因此结果会处于tainted状态。

### define vs attach

见<https://rmculpepper.github.io/blog/2013/06/define-vs-attach/>

### 为什么需要define-match-expander

因为会需要定义的名字在match内外有不同的行为。例如，在match外面使用时，直接报错。

因此需要把match-expander定义成一个struct，并利用struct properties控制其行为。prop:procedure对应在外面使用的情况，prop:match-expander对应match内使用的情况。

那么为什么需要prop:match-expander，而不是固定使用某一字段呢？

因为struct可以有多个property，可以同时带有prop:pattern-expander等其他的，做到一个名字在不同宏里有不同意义。如果简单限定为一个字段，嵌套后会失去其他属性。

