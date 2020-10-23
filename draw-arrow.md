# 如何让DrRacket正确地画出箭头

在DrRacket里，当光标移动到一个名字上时，会出现从其使用指向其定义的箭头。这个箭头可以辅助代码阅读，也预示了“变量重命名”功能的作用范围。

由于宏的存在，一部分的 _identifier_ 会在宏返回的syntax对象中丢失，因此需要在宏返回的syntax对象的 _syntax property_ 里追加相应的信息。涉及的 _syntax property_ 有：

* `disappeared-use` 

* `disappeared-binding`
* `sub-range-binders` 
* `origin`
* `original-for-check-syntax`

对宏编写者而言，`disappeared-use`属性动得最频繁。所有不出现在宏返回的syntax对象中的 _identifier_ ，都应该被记录 _syntax property_ 的这一项里（除了宏自己的名字的 _identifier_ ，那个是由expander记录到`origin`属性）。

现在看一下几种常见的情况。

## 宏的Pattern 的 literal identifier

`syntax-rules`、`syntax-case`等的pattern里面的 _literal identifier_ ，是`disappeared-use`属性遗漏的重灾区（截至7.8，`case`宏仍不能给`else`的使用画上箭头）。

下面这个程序非常简单，但是`foo`的使用却画不出箭头：

```racket
#lang racket

(define-syntax foo (syntax-rules ()))

(define-syntax bar
  (syntax-rules (foo)
    [(_ foo x) x]))

(bar foo 1)
```

所以，`syntax-rules`是不能自动处理好这个问题的。当需要匹配syntax中的 _literal identifier_ 时，不要用`syntax-rules`。

`(syntax-rules () _ ...)`以外的用法都是不恰当的。

先考虑换成`syntax-case`：

```racket
(define-syntax (bar stx)
  (syntax-case stx (foo)
    [(_ foo x)
     #'x]))
```

这里有一个麻烦的地方，`syntax-case`不会为pattern中的 _literal identifier_ 引入 _pattern variable_ ，不能直接用 `#'foo` 访问到用户输入的`foo`。因此要变通一下：

```racket
(define-syntax (bar stx)
  (syntax-case stx ()
    [(_ foo-id x)
     (free-identifier=? #'foo-id #'foo)
     #'x]))
```

这里选择用`syntax-case`的"fender-expr"来对 _literal identifier_ 进行匹配，这样`#'foo-id`就是用户输入的`foo`了。

然后是添加`disappeared-use`：

```racket
(define-syntax (bar stx)
  (syntax-case stx ()
    [(_ foo-id x)
     (free-identifier=? #'foo-id #'foo)
     (syntax-property #'x
                      'disappeared-use
                      (list (syntax-local-introduce #'foo-id)))]))
```

这里的`syntax-local-introduce`是必要的，因为宏展开结束反转 _scope_ 的时候不会深入到 _syntax property_ 里面的 _identifier_ 。为了让`foo`能被正确识别为原始输入的一部分，需要手动用`syntax-local-introduce`反转 _scope_ 。

### syntax-parse

另一方面，`syntax-parse`支持`#:track-literals`选项，这种情况的处理就非常简单了：

```racket
(define-syntax-parser bar #:track-literals
  [(_ (~literal foo) x) #'x])
```

可以看出`syntax-parse`的巨大优势。因此在编写宏时，能用`syntax-parse`的应该尽量用。

## "Pattern Expander"的Pattern中的literal identifier

对于模拟单步的宏展开的"pattern expander"（见[可扩展的宏](https://github.com/yjqww6/macrology/blob/master/Extensible%20Macros.md)），情况要稍微复杂一些。有几种情况：

### 非表达式的位置

```racket
#lang racket
(require (for-syntax syntax/apply-transformer)
         syntax/parse/define)

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx 'expression)))

(define-syntax foo (syntax-rules ()))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     #`(let (#,(apply-expander (syntax-local-value #'id) #'in))
         (void))]))

(define-syntax-parser expander1 #:track-literals
  [(~literal foo) #'[x 1]])

(use-expander expander1 foo)
```

这里，`expander1`的结果没有放在表达式位置，即便添加了`#:track-literals`，也没有用。

这种情况可以用`with-disappeared-uses`：

```racket
(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     (with-disappeared-uses
         (record-disappeared-uses #'id)
       #`(let (#,(apply-expander (syntax-local-value #'id) #'in))
           (void)))]))

(define-syntax-parser expander1
  [(~and (~literal foo) foo-id)
   (record-disappeared-uses #'foo-id)
   #'[x 1]])
```

这样，`expander1`和`foo`都画上了箭头。

### 非local-apply-transformer

把上面的`apply-expander`定义换成：

```racket
(define (apply-expander proc stx)
  (define introducer (make-syntax-introducer))
  (define intro-stx (introducer (syntax-local-introduce stx)))
  (syntax-local-introduce (introducer (proc intro-stx))))
```

这种旧式的展开方法因为`record-disappeared-uses`默认的`syntax-local-introduce`不是上面的`introducer`，所以画不出`foo`的箭头。

需要提供正确的`introducer`，并让`record-disappeared-uses`不进行`syntax-local-introduce`：

```racket
#lang racket
(require (for-syntax racket/syntax)
         syntax/parse/define)

(begin-for-syntax
  (define current-introducer (make-parameter #f))
  (define (current-introduce x)
    ((current-introducer) x))
  
  (define (apply-expander proc stx)
    (define introducer (make-syntax-introducer))
    (define intro-stx (introducer (syntax-local-introduce stx)))
    (syntax-local-introduce
     (introducer
      (parameterize ([current-introducer introducer])
        (proc intro-stx))))))


(define-syntax foo (syntax-rules ()))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     (with-disappeared-uses
         (record-disappeared-uses #'id)
       #`(let (#,(apply-expander (syntax-local-value #'id) #'in))
           (void)))]))

(define-syntax-parser expander1
  [(~and (~literal foo) foo-id)
   (record-disappeared-uses (current-introduce #'foo-id) #f)
   #'[x 1]])

(use-expander expander1 foo)
```

### 别人写的宏

如果上面的`use-expander`不能改，而`expander1`能改，那就要在`expander1`里寻找一个表达式位置了：

```racket
(define-syntax-parser expander1
  [(~and foo-id (~literal foo))
   #:with expr
   (syntax-property #'1
                    'disappeared-use
                    (list (syntax-local-introduce #'foo-id)))
   #'[x expr]])
```

若是非`local-apply-transformer`的情况（例如`for`），`syntax-local-introduce`不适用，可以改为用宏延迟 _syntax property_ 的添加：

```racket
(define-syntax-parser disappeared-use
  [(_ x:id ...)
   (syntax-property
    #'(void)
    'disappeared-use
    (map syntax-local-introduce
         (syntax->list #'(x ...))))])

(define-syntax-parser expander1
  [(~and foo-id (~literal foo))
   #'[x (begin (disappeared-use foo-id) 1)]])
```

## 其他情况

* 如果要把`local-expand`的结果拆出一部分，原有的 _syntax property_ 可能会遗失。
* 像`struct`那样引入名字由多个输入组合而成的定义的情况，需要添加`sub-range-binders`属性。

参考[如何使用First Class Internal Definition Context](https://github.com/yjqww6/macrology/blob/master/intdef-ctx.md)：

```racket
    (define-syntax-rule (syntax/track form)
      (syntax-case this-syntax ()
        [(head . _) (syntax-track-origin #'form this-syntax #'head)]))]
 ...
       [(begin form ...)
        #:with (expanded-form ...) (stx-map loop #'(form ...))
        (syntax/track (begin expanded-form ...))]
 ...
```

这里使用了`syntax-track-origin`来复制原有的 _syntax property_ ，而原来的`begin`则被添加到`origin`属性中了。如果要继续添加`disappeared-use`属性，需要与原来的属性组合，类似于：

```racket
(syntax-property v
 'disappeared-use
 (cons (syntax-local-introduce #'id)
       (or (syntax-property v 'disappeared-use) null)))
```



至于`sub-range-binders`属性的用法比较简单，可以直接看The Racket Reference，一般用`format-id`的`#:subs?`特性即可。



其他情况也有，但由于不常见，这里不展开讨论。

* 如果使用 _first class internal definition context_ 展开时，引入的定义不出现在结果中，可以用`internal-definition-context-track`。
* 类似于`syntax-parse`的`xx:id`的情况，这里由于没有一个用户提供的`xx`或`id`，会需要手动构造带有恰当的源码位置信息的 _identifier_ ，并且添加`original-for-check-syntax`属性。

## Arrow Art

用这些箭头画一些图案的行为叫做Arrow Art，示例（需要通过右键 -> "Tack/Untack Arrow(s)"固定箭头看到效果）：

```racket
#lang racket
(define-syntax (arrow-art stx)
  (syntax-case stx ()
    [(_ id ...)
     (syntax-property
      (syntax-property
       #'(void)
       'disappeared-use
       (map syntax-local-introduce (syntax->list #'(id ...))))
      'disappeared-binding
      (map syntax-local-introduce (syntax->list #'(id ...))))]))

(arrow-art a     a
          
              a)

(arrow-art b     b

              b


              b)
```

