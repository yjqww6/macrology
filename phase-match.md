# Phase Level 与 identifier的匹配

在比较`a`的值是否和某个特定的 _identifier_ `#'b`相同时，一般会直接地（或通过`syntax-rules`等间接地）使用`(free-identifier=? a #'b)`。但也有一些情况不能如此处理。

在讨论 _phase level_ 与 _identifier_ 的匹配之前，首先要明确几个概念（在The Racket Reference中有更加详细的解释）。

* 一个 _identifier_ 在不同的 _phase_ 可以拥有不同的 _scope set_ ，可以解析到相同或不同的 _binding_ ，或者干脆没有 _binding_ 。
* module的 _base phase_ 是其实例化所在的 _phase level_ 。进行`require`的时候，是在相对于 _base phase_ 的环境里引入 _binding_ ：直接的require就是在 _base phase_ ，`for-template`就是 _base phase_ - 1，以此类推……
* `free-identifier=?`比较的是两个 _identifier_ 在给定的 _phase level_ 是否有相同的 _binding_ （或者都没有 _binding_ ），而默认的 _phase level_ 是`(syntax-local-phase-level)`。

由上可知，`(free-identifier=? a #'b)`是否正确，一个关键是`(syntax-local-phase-level)`是否就是所期望的 _phase level_ 。



## syntax-local-phase-level的影响

根据The Racket Reference的解释：

```
During the dynamic extent of a syntax transformer application by the expander, the result is the phase level of the form being expanded. Otherwise, the result is 0.
```

所以这里就有两种特殊情况了：

* “phase level of the form being expanded” 和目标的 _identifier_ 所在的 _phase level_ 不一致。
* 不是在做宏展开。



注：`syntax-local-phase-level`是有可能得到负数的：

```racket
> (module a racket
    (begin-for-syntax
      (displayln (syntax-local-phase-level))))
0
0
0
> (require (for-template 'a))
-1
```



### Phase Level 不一致的情况

```racket
#lang racket

(module a racket
  (define-for-syntax f 1)
  
  (define-syntax do
    (syntax-rules (f)
      [(_ f) (begin-for-syntax (displayln 1))]
      [(_ form) (begin-for-syntax (displayln 0))]))

  (provide do (for-syntax f)))

(require 'a)

(define g 0)
(do f)
```

这个程序中，a的`do`宏需要将输入与其上面定义在 _phase level_ 1的`f`比较。但是，被展开的代码在0（即`(syntax-local-phase-level)`为`0`）。

`(do f)`中的`f`在 _phase level_ 0没有 _binding_ ，`(syntax-rules (f) ...)`的`f`在 _phase level_ 0也没有 _binding_ 。因此，打印的是`1`。

然而，如果将`(define g 0)`中的`g`换成`f`，那么`(do f)`中的`f`在 _phase level_ 0解析到这个 _binding_ 。而`(syntax-rules (f) ...)`的`f`仍然没有 _binding_ ，结果就是`0`了。



所以，这里的实际问题是`syntax-rules`没有在 _phase level_ 1比较 _identifier_ 。修改方法有：

```racket
(define-syntax (do stx)
  (syntax-case* stx (f) free-transformer-identifier=?
    [(_ f) #'(begin-for-syntax (displayln 1))]
    [(_ form) #'(begin-for-syntax (displayln 0))]))
```

或

```racket
(define-syntax (do stx)
  (syntax-case stx ()
    [(_ f-id)
     (free-transformer-identifier=? #'f #'f-id)
     #'(begin-for-syntax (displayln 1))]
    [(_ form)
     #'(begin-for-syntax (displayln 0))]))
```

`free-transformer-identifier=?`在`(add 1 (syntax-local-phase-level))`进行比较。两边的`f`在 _phase level_ 1有相同的 _binding_ ，不论外面是不是有 _phase level_ 0的`f`干扰，都能不受影响输出`1`。

### 非宏展开的情况

看如下程序：

```racket
#lang racket
(module foo racket/base
  (define-syntax foo (syntax-rules ()))
  (provide foo))

(module a racket/base
  (require (for-template (submod ".." foo)))
  
  (define foo "bad")
  
  (define (is-foo? stx)
    (syntax-case stx (foo)
      [foo #t]
      [_ #f]))
  (provide is-foo?))

(require 'foo (for-syntax 'a))

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id)
     (is-foo? #'id)
     #'#t]
    [_ #'#f]))

(f foo)
```

在module a中，定义了一个辅助函数`is-foo?`，用来判断一个 _identifier_ 是不是解析到module foo的`foo`。

`is-foo?`运行在 _phase level 1_，因为有`for-template`，a的`syntax-case`的`foo`在 _phase level_ 1 - 1 = 0 解析到module foo的`foo`。而a定义的`foo`在 _phase level_ 1，因此不会产生干扰。

`(f foo)`的`foo`因为在 _phase level_ 0 require了module foo，所以解析到module foo的`foo`。

因此二者 _binding_ 相同，结果是`#t`。



然而，如果要在非宏展开的时候使用这两个module，情况就不一样了：

```racket
#lang racket
(module foo racket/base
  (define-syntax foo (syntax-rules ()))
  (provide foo))

(module a racket/base
  (require (for-template (submod ".." foo)))
  
  (define foo "bad")
  
  (define (is-foo? stx)
    (syntax-case stx (foo)
      [foo #t]
      [_ #f]))
  (provide is-foo?))

(require 'foo 'a)

(is-foo? #'foo)
```

此时`(syntax-local-phase-level)`仍为0，而module a的`foo`定义在0，module foo的`foo`在”-1“。这样一来`syntax-case`把`stx`（仍然解析到module foo的`foo`）与module a定义的`foo`相比较，得到`#f`。



就结果而言：

* 在第一个例子中，module a的 _base phase_ 为1，`is-foo?`比较了`stx`和module foo的`foo`。

* 在第二个例子中，module a的 _base phase_ 为0，`is-foo?`比较了`stx`和module a自己的`foo`。

也就是说，在不改动module a和module foo的情况下，仅仅是使用方式的不同，就使`is-foo?`产生了完全不同的行为。

解决这个问题的关键是计算module a的 _base phase_ （使用`variable-reference->module-base-phase`）：

```racket
(module a racket/base
  (require (for-template (submod ".." foo)))
  
  (define foo "bad")

  (define base-phase
    (variable-reference->module-base-phase
     (#%variable-reference)))
  
  (define (is-foo? stx)
    (free-identifier=?
     stx #'foo
     (syntax-local-phase-level)
     (sub1 base-phase)))
  
  (provide is-foo?))
```

这样，`is-foo?`总是比较`stx`和module foo的`foo`。

（另见<https://rmculpepper.github.io/blog/2011/09/syntax-parse-and-literals/>）



## 匹配Fully Expanded Program

_Fully Expanded Program_ （以下简称FPE）中的 _identifier_ 的 _phase level_ 情况也比较微妙。当然，这里的FPE指的是非 _expression context_ 下的FPE——在 _expression context_ 下，FPE并不涉及任何 _phase level_ 的变化，属于平凡的情况。

非 _expression context_ 的FPE通常有两种来源：

* 对一个 _top-level_ 的module做完全展开。

* `#%module-begin`对其body做完全展开。

值得注意的是，匹配一个完全展开的 _top-level_ module的情况，是完全可能发生在非宏展开的时候的——各种annotator工作的基本流程就是：expand -> annotate（这里需要进行匹配） -> compile/eval。这个annotate过程正是发生在expander结束工作之后。



要匹配FPE中的 _identifier_ ，假定表达式是`(free-identifier=? id lit phase lit-phase)`。

这里`id`是输入的 _identifier_ ， `lit`是程序指定的 _literal identifier_ （应当属于`(kernel-form-identifier-list)`）， 这两个参数没有疑问。而`lit-phase` 需要保证`lit`带有正确的 _binding_ ，取值参照前一节即可。

所以关键就是`phase`的取值了。

### phase的初值

根据上面的两种情况，匹配的起点可能是`module`或者`#%plain-module-begin`。

* `module`的情况，取决于FPE的来源：
  * 如果是通过`expand`/`expand-syntax`展开得到的，那么`phase`的初值应当是`(namespace-base-phase)`；
  * 通过`local-expand`（`context-v`为`'top-level`）得到。虽然比较罕见，但也是有可能的。`phase`的初值应为`(syntax-local-phase-level)`。并且，非宏展开的情况用不了`local-expand`，`lit-phase`也不用考虑非宏展开的情况。
* `#%plain-module-begin`肯定是`local-expand`得到的，所以是`(syntax-local-pahse-level)`。但由于module的body总是从 _phase level_ 0开始，所以也可以直接用`0`。

### phase的变化

除了下面提到的情况外，`phase`都保持不变

* `begin-for-syntax`和`define-syntaxes`：比较简单，这两个 _identifier_ 本身在`phase`匹配，但对于`begin-for-syntax`的”body“和`define-syntaxes`的”expr“，`phase`需要加1。

* `module`和`module*`：这两个 _identifier_ 本身在`phase`匹配，但对于其”body“，`phase`为`0`。
  
  * `(module* _ #f _)`的情况则是特例，虽然其”body“仍是在 _phase level_ 0展开，但最终得到的FPE经过了shift，所以`phase`无需变动。
  
  需要注意的是，对于这些submodule的`#%plain-module-begin`，根据module的嵌套的情况，`phase`的变动不固定。但由于`#%plain-module-begin`的位置相对`module`/`module*`固定，所以也并不需要去匹配——直接匹配`module`/`module*`，然后跳过`#%plain-module-begin`处理其”body“即可。

### kernel-syntax-case/phase

对于匹配FPE，`kernel-syntax-case/phase`是最方便的工具，可以自动处理好 _literal identifier_ 和其 _phase level_ ，用户只需要提供`phase`参数即可。

如果不需要考虑进入 _phase level_ 大于0的部分，`kernel-syntax-case`也可以用。

### 示例

`drcomplete-user-defined`库里有一段非常简短的代码，可以作为示例，这里就不再另外提供示例代码。

<https://github.com/yjqww6/drcomplete/blob/master/drcomplete-user-defined/private/main.rkt>