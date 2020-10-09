# local-expand该怎么用

## 什么时候需要用local-expand
* 什么时候会用到`local-expand`?

  当需要对展开结果立即进行进一步操作的时候。

* 什么时候要用`local-expand`，而不是`local-apply-transformer`？

  如果要展开的对象是一个可以在指定的上下文里直接运行的定义或表达式，则需要用`local-expand`；如果是一些不能运行的pattern，用`local-apply-transformer`。

## 参数怎么选

### 局部的完全展开与Expression Context

当`stop-ids`参数选择`null` 时，`context-v`一般是`'expression`。也就是说，局部的完全展开需要 _expression context_ 。而如果当前的`(syntax-local-context)`不是`'expression`时，则需要延迟到 _expression context_ 。

```rack
#lang racket

(define-syntax (local stx)
  (syntax-case stx ()
    [(_ e)
     (local-expand #'e 'expression null)]))

(new
 (class object%
   (super-new)
   (define/public (a) 1)
   (local (displayln (a)))))
;;; class: misuse of method (not in application) in: a
```

这个例子中，`local`的展开是一个 _internal definition context_ ，意味着环境还没有设置完毕，但却进行了完全展开，因此出现问题。

正确写法：用`#%expression`延迟到 _expression context_ 再展开

```rac
#lang racket

;;;也可以直接用make-expression-transformer
(define-syntax (local stx)
  (syntax-case stx ()
    [(_ e)
     (eq? (syntax-local-context) 'expression)
     (local-expand #'e 'expression null)]
    [form
     #'(#%expression form)]))

(new
 (class object%
   (super-new)
   (define/public (a) 1)
   (local (displayln (a)))))
```

那么什么时候要用功能类似的`syntax-local-expand-expression`呢？情况可以分为以下几种

* 需要改写结果

  用`local-expand`。

* 结果不变，但需要放进其他binding form里，导致引入新的local _scope_ 。例如`let`的`body`和`letrec`的`body`及`rhs`。

  用`local-expand`。

  典型例子是`place/context`：其会用`local-expand`展开`body`，再计算出其中的自由变量。而展开的`body`放进一些let之类的东西里面，使得这些名字在结果里会重新绑定到place channel传进来的参数里，意义发生了变化。因此这种情况不适用`syntax-local-expand-expression`。

* 结果不变，直接返回或在不引入新的local _scope_ 情况下返回。

  用`syntax-local-expand-expression`。至于是不是`opaque-only?`，则看是否需要对结果进行其他操作。

### 其他的完全展开

一般是在实现新的`#%module-begin`时使用，此时`context-v`是`'module-begin`，`stop-ids`一般是`(list #'module*)`或`null`。

### 部分展开

部分展开的情况就比较混乱了，没有固定的用法，这里只能简单总结一下。

* 在 _internal definition context_ 展开的情况，一般是配合 _first class internal definition context_ 使用，`stop-ids`至少应包括`define-values`、`define-syntaxes`和`begin`。详细可见[如何使用First Class Internal Definition Context](https://github.com/yjqww6/macrology/blob/master/intdef-ctx.md)。

## 什么时候要对local-expand的结果用syntax-disarm

在对结果进行操作时，如果只对最外层的名字进行检测不把拆开的结果返回，或者只拆开`begin` `begin-for-syntax` `#%plain-module-begin` `define-values` `define-syntaxes`，则不需要`syntax-disarm`。

如果还要再进一步拆开里面的syntax对象，需要先对其使用`syntax-disarm`，再对返回值使用`syntax-rearm`。`inspector`使用module声明时的inspector，可通过`(variable-reference->module-declaration-inspector (#%variable-reference))`获得。

## 使用示例

如果要写这样一个宏`(let-box ([x rhs]) body ...)`：如果`x`在`body`里没有被`set!`过，那么就跟普通的`let`一样；如果被`set!`过了，那么`x`被放到box里，并且所有对x的引用变成`unbox` ，所有对`x`的赋值变成`set-box!`。

应该怎么写呢？

1. `x`有没有`set`!过，需要`body`完全展开之后才能得知，所以需要用`local-expand`进行完全展开

2. 需要完全展开，所以延迟到 _expression context_ 
3. 要记录`set!`的情况，可以把x的名字绑定到一个 _set!-transformer_ ，在里面通知`let-box`。这个过程可以用 _3d syntax_ 简单地完成。
4. 因为没有被`set!`的情况下等于普通的`let`，可以利用`syntax-local-expand-expression`进行优化。
5. 被`set!`过了的话，要对展开结果进行修改，因此需要`syntax-disarm`。

终上所述，`let-box`写出来如下：

```racket
#lang racket
(require syntax/parse/define)

(define-syntax-parser let-box
  [(_ ([x:id rhs]) body ...+)
   #:when (eq? (syntax-local-context) 'expression)
   #:do [(define setted? #f)]
   #:with proc (datum->syntax #f (λ () (set! setted? #t)))
   #:do [(define-values (expanded opaque) 
           (syntax-local-expand-expression
            #'(let ([x rhs])
                (let-syntax ([x (make-set!-transformer
                                 (lambda (stx)
                                   (syntax-case stx (set!)
                                     [(set! id v) (begin ('proc) #'(set! x v))]
                                     [id (identifier? #'id)  #'x])))])
                  body ...))))]
   (cond
     [(not setted?) opaque]
     [else
      (define insp
        (variable-reference->module-declaration-inspector
         (#%variable-reference)))
      (syntax-case (syntax-disarm expanded insp) (let-values)
        [(let-values ([(x) rhs]) body ...)
         (syntax-rearm
          #'(let ([x (box rhs)])
              (let-syntax ([x (make-set!-transformer
                               (lambda (stx)
                                 (syntax-case stx (set!)
                                   [(set! id v) #'(set-box! x v)]
                                   [id (identifier? #'id)  #'(unbox x)])))])
                body ...))
          expanded)])])]
  [form
   #'(#%expression form)])
```

使用如下：

```racket
> (let-box ([x 1])
           x)
1
> (let-box ([x 1])
           (set! x 2)
           x)
2
> (syntax->datum (expand '(let-box ([x 1])
                                   x)))
'(#%expression (let-values (((x) '1)) (let-values () (let-values () x))))
> (syntax->datum (expand '(let-box ([x 1])
                                   (set! x 2)
                                   x)))
'(#%expression
  (let-values (((x) (#%app box '1)))
    (let-values ()
      (let-values () (let-values () (let-values () (#%app set-box! x '2) (#%app unbox x)))))))
```

