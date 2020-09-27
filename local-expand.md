# local-expand该怎么用

## 什么时候需要用local-expand
* 什么时候会用到local-expand?

  当需要对展开结果立即进行进一步操作的时候。

* 什么时候要用local-expand，而不是local-apply-transformer？

  如果要展开的对象是一个可以在指定的上下文里直接运行的定义或表达式，则需要用local-expand；如果是一些不能运行的pattern，用local-apply-transformer。

## 参数怎么选

### 局部的完全展开与Expression Context

当stop-ids参数选择`null` 时，context-v一般是`'expression`。也就是说，局部的完全展开需要expression context。而如果当前的`(syntax-local-context)`不是`'expression`时，则需要延迟到expression context。

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

这个例子中，local的展开是internal definition context，意味着环境还没有设置完毕，但却进行了完全展开，因此出现问题。

正确写法：用#%expression延迟到expression context再展开

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

那么什么时候要用功能类似的syntax-local-expand-expression呢？情况可以分为以下几种

* 需要改写结果

  用local-expand。

* 结果不变，但需要放进其他binding form里，导致引入新的local scope。例如let的body和letrec的body及rhs。

  用local-expand。

  典型例子是place/context：其会用local-expand展开body，再计算出其中的自由变量。而展开的body放进一些let之类的东西里面，使得这些名字在结果里会重新绑定到place channel传进来的参数里，意义发生了变化。因此这种情况不适用syntax-local-expand-expression。

* 结果不变，直接返回或在不引入新的local scope情况下返回。

  用syntax-local-expand-expression。至于是不是opaque-only?，则看是否需要对结果进行其他操作。

### 其他的完全展开

一般是在实现新的#%module-begin时使用，此时context-v是`'module-begin`，stop-ids一般是`(list #'module*)`。

### 部分展开

部分展开的情况就比较混乱了，没有固定的用法，这里只能简单总结一下。

* 在internal definition context展开的情况，一般是配合first class internal definition context使用，stop-ids至少应包括define-values、define-syntaxes和begin。

## 什么时候要对local-expand的结果用syntax-disarm

在对结果进行操作时，如果只对最外层的名字进行检测不把拆开的结果返回，或者只拆开`begin` `begin-for-syntax` `#%plain-module-begin` `define-values` `define-syntaxes`，则不需要syntax-disarm。

如果还要再进一步拆开里面的syntax object，需要先对其使用syntax-disarm，再对返回值使用syntax-rearm。inspector使用module声明时的inspector，可通过`(variable-reference->module-declaration-inspector (#%variable-reference))`获得。