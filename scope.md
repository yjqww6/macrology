# Scope和Binding

Racket的宏系统是基于Binding as Sets of Scopes算法的，在实现一些复杂的宏时，往往会需要对宏进行手动操纵。本文将对Racket中的各种scope进行分析，以期建立一个清晰的心理模型，能快速理解Macro Stepper的输出，对宏的各种scope的问题进行调试并修复。

（注：阅读本文前，应了解[Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/)的1-3节内容）

## Scope盘点

Racket的 _identifier_ 在不同的 _phase_ 可以拥有不同的scope，解析到不同的 _binding_ ，而`syntax-shift-phase-level`可以偏移各 _phase_ 的scope。但实际上，大部分的scope都是作用于所有 _phase_ 的，也就是说，“shift”对其不起作用。

### Phase特定的Scope

_phase_ 特定的scope有且仅有一种，是在展开module或top-level的时候引入的。这种scope在expander里被称为"multi scope"，`syntax-shift-phase-level`仅对这部分scope起作用。严格来说，“multi scope”并不是通常意义上的scope，它是一种lazy的结构，在各个 _phase_ 体现为不同的“representation scope”——这个“representation scope”更接近于通常意义上的scope。

运行以下程序

```racket
#lang racket
(define a (make-base-namespace))

(hash-ref
 (syntax-debug-info
  (namespace-syntax-introduce
   (datum->syntax #f 'a) a))
 'context)

(hash-ref
 (syntax-debug-info
  (syntax-shift-phase-level
   (namespace-syntax-introduce
    (datum->syntax #f 'a) a)
   -1))
 'context)
```

可以看到类似

```racket
'(#(0 module) #(86798 module top-level))
'(#(0 module) #(86799 module top-level))
```
的字样。这里的`#(86798 module top-level)`和`#(86799 module top-level)`就是a命名空间里，top-level的“multi scope”，所对应的 _phase level_ 0和1的“representation scope”了。

### Phase无关的Scope
对于 _phase_ 无关的各种scope，其自身属性并无区别，主要是按用途来归类。（实际上，也存在interned和uninterned的区别，但由于 _interned scope_ 极少会被用到，这里按下不表）

按`syntax-debug-info`显示的名字来分，有以下几种scope：

* local

  _Fully Expanded Program_ 的 _binding form_ （`let-values`、`#%plain-lambda`等）所引入的scope，用于区分local的 _binding_ 。

* macro

  宏展开引入的scope，通过宏展开前后的两次反转，所有展开过程中新引入的syntax对象都会添加上该scope。

* use-site

  当一个宏的定义和使用在同一个 _definition context_ 时，宏的参数会带上该scope。

* module

  展开module时引入的scope。

* intdef

  展开 _internal definition context_ 时引入的scope。

* letrec-body 

  `letrec-values`/`letrec-syntaxes+values`添加到其“body”（“rhs”没有）的scope。虽然Binding as Sets of Scopes提到“body-scope”机制在Racket中没有采用，但事实上还是用到了：

  ```racket
  #lang racket
  (define-syntax (f stx)
    (syntax-case stx ()
      [(_ id)
       (displayln (hash-ref (syntax-debug-info stx) 'context))
       #'(void)]))
  
  (letrec-values ()
    (f a))
  ```

  ```racket
  (#(43768 module) #(43775 module anonymous-module) #(43836 local) #(43837 intdef) #(43838 local) #(43839 letrec-body) #(43840 intdef) #(43841 macro))
  ```

  可以看到这里的`#(43839 letrec-body)`。

### inside-edge scope和outside-edge scope

在 _definition context_ 展开的时候，输入的syntax对象会带上 _outside-edge scope_ 和 _inside-edge scope_ ；并且，展开的结果也会带上 _inside-edge scope_ 。

_outside-edge scope_ 区分宏引入的 _identifier_ ， _inside-edge scope_ 区分不同的 _definition context_ 。

这两种scope并不是新种类的scope，而是从另一个维度对上述的各种scope进行分类。



对于各种 _definition context_ ：

* top-level的情况：充当 _outside-edge scope_ 的是一个所有top-level共享的scope，即上文出现的`#(0 module)`；充当 _inside-edge scope_ 的是一个特定的“multi scope”。`namespace-syntax-introduce`就是添加这对scope。

* module的情况：充当 _outside-edge scope_ 的是特定的module scope；充当 _inside-edge scope_ 的是一个特定的“multi scope”。

* _internal definition context_ 的情况：充当 _inside-edge scope_ 的是特定的intdef scope；充当 _outside-edge scope_ 的是外面的 _binding form_ 特定的local scope，以及letrec-body scope（如果存在）。

* _first class internal definition context_ 的情况和上面类似——除了没有 _outside-edge scope_ ，这也导致了一些卫生问题。

  一个来自于<https://github.com/racket/racket/issues/3198>的例子：

  ```racket
  #lang racket
  
  (define x 'good)
  (define-syntax-rule (m) (displayln x))
  
  (define c%
    (class object%
      (super-new)
      (define x 'bad2)
      (m)))
  
  (new c%)
  ```

  在Racket 7.8中，输出`bad2`。因为`bad2`和`m`中的`x`展开后都被打上了作为 _inside-edge scope_ 的intdef scope，但由于没有 _outside-edge scope_ ，情况变成了：

  1. good x common scopes
  2. bad2 x：common scopes + intdef scope
  3. m x：common scopes + intdef scope + macro scope

  workaround是给它凑上一个 _outside-edge scope_ ，例如使用一个local scope：

  ```racket
  (define c%
    (let ()
      (class object%
        (super-new)
        (define x 'bad2)
        (m))))
  ```

  这样bad2 x多了个scope，不会绑定m x。

  实际上从这个问题中可以看到 _outside-edge scope_ 和 _use-site scope_ 的相似性。



另一个需要注意的是，由于“multi scope”也被用作 _inside-edge scope_ ，如果进行了shift，被移走的当前 _phase_ 的“representation scope”在后面又会加回来——最终该 _identifier_ 仍能触及当前 _phase_ 的 _binding_ ，并且在当前的 _phase_ 带有两种“representation scope”。因此下面这个程序不会出现“unbound identifier”错误。

```racket
#lang racket

(define x 1)

(define-syntax (m stx)
  (syntax-case stx ()
    [(_ id)
     (syntax-shift-phase-level #'id -1)]))

(m x)
```



---

盘点完scope后，再来看看scope和binding的关系。

## Scope和Binding

Binding as Sets of Scopes中提到

```
extends a global table that maps a ⟨symbol, scope set⟩ pair to a representation of a binding.
```

那么Racket中存在这么一张全局的表吗？按照常识，正经的实现里肯定不会出现一个这么容易内存泄漏的结构。这里就可能导致一些误区：

* 认为 _identifier_ 捕获了自身所在的局部环境——实际上 _identifier_ 仍然只是 symbol + scope set。

* 认为解析一个 _identifier_ 的 _binding_ 依赖当前环境——实际上`identifier-binding`这个函数并不需要一个namespace参数。

  ```racket
  #lang racket
  
  (define x (datum->syntax #f 'x))
  
  (define ns (make-base-namespace))
  (define add-scope (make-syntax-delta-introducer
                     (namespace-syntax-introduce x ns)
                     x))
  
  (eval-syntax #`(define #,x 1) ns)
  
  (identifier-binding (add-scope x) 0 #t)
  ```

  这里`identifier-binding`看起来不可能知道ns的存在，但仍然能解析到ns中的binding，得到`'(x.1)`。

---

这些看起来暗示着这么一张全局的表的存在，但实际上Racket使用的是一个等效的结构：scope反过来索引了所有包含了该scope的binding。因此在分析问题时，可以简单地假定这张表存在。

### binding的解析

Sets of Scopes下的binding解析就是寻找scope set的greatest子集而已，找不到任何子集就“unbound identifier”，找到多个maximal子集就“identifier's binding is ambiguous”。

但是，“multi scope”的的处理就比较特殊——如果解析失败，它会把最近加入的“multi scope”去掉再尝试，直到没有“multi scope”为止。

```racket
#lang racket

(define a-ns (make-base-namespace))
(define b-ns (make-base-namespace))
(define x (datum->syntax #f 'x))

(for ([ns (in-list (list a-ns b-ns))]
      [i (in-naturals)])
  (define name 
    (datum->syntax #f (string->symbol (format "ns-~a" i))))
  (eval-syntax #`(define #,name #t) ns)
  (eval-syntax #`(define-syntax #,x (make-rename-transformer
                                     #'#,name))
               ns))

(define (resolve . nss)
  (identifier-binding
   (for/fold ([x x])
             ([ns (in-list nss)])
     (namespace-syntax-introduce x ns))
   0
   #t))

(resolve a-ns b-ns)

(resolve b-ns a-ns)
```

输出`'(ns-0.1)`和`'(ns-1.1)`。这就是“multi scope”添加顺序对binding解析的影响。

这个设计是为了方便syntax对象在多个命名空间里使用。

## 实例：仿local-expand/capture-lifts

Racket的`local-expand/capture-lifts`可以用来分隔不同的语言，但若想要用于其他的用途，就不太适合了。因为它会无条件捕获其他的lift，与正常的使用相互干扰。

这里通过模仿`local-expand/capture-lifts`来展示如何维护正确的scope。

### 基本框架

首先确定api：

```racket
(define (lift expr) <...>)
(define (local-expand/capture stx [intdef-ctx '()]) <...>)
```

基本上和`syntax-local-lift-expression`和`local-expand/capture-lifts`类似，为了简化，这里限定只做expression context下的完全展开。

然后，`lift`需要添加binding，Racket中能动态添加binding的机制就是first class intdef-ctx。所以`local-expand/capture`需要告诉`lift`目标的first class intdef-ctx，并且记录`expr`，在展开结束后填到返回的syntax对象里。

可以用parameter来指示记录的位置：

```racket
(define current-lift-context
    (make-parameter #f))
```

`current-lift-context`的类型是
```racket
(Parameter (Pairof Internal-Definition-Context 
                   (Boxof (Listof (Pairof Identifier Syntax))))
```

填写`lift`和`local-expand/capture`的实现和测试代码：

```racket
#lang racket
(module a racket/base
  (require (for-template racket/base)
           racket/match)
  
  (provide (all-defined-out))
  
  (define current-lift-context
    (make-parameter #f))

  (define (lift expr)
    (match-define (cons ctx b) (current-lift-context))
    (define id
      (car (generate-temporaries (list expr))))
    (syntax-local-bind-syntaxes
     (list id) #f ctx)
    (set-box! b
              (cons (list id expr)
                    (unbox b)))
    id)

  (define (local-expand/capture stx [intdef-ctx '()])
    (define ctx (syntax-local-make-definition-context))
    (define b (box '()))
    (define expanded
      (parameterize ([current-lift-context (cons ctx b)])
        (local-expand stx 'expression null (cons ctx intdef-ctx))))
    
    (with-syntax ([([id expr] ...) (reverse (unbox b))]
                  [expanded expanded])
      #'(let ()
          (define id expr)
          ...
          (let ()
            expanded)))))

(require (for-syntax 'a syntax/transformer))


(define-syntax (liftme stx)
  (syntax-case stx ()
    [(_ a b c)
     (begin
       (define a1 (lift #'a))
       (define b1 (lift #'b))
       (define c1 (lift #'c))
       #`(list #,a1 #,b1 #,c1))]))

(define-syntax capme
  (make-expression-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ form)
        (local-expand/capture #'form)]))))

(capme
 (apply +
        (liftme (add1 0) 2 3)))
```

得到`identifier used out of context: #<syntax temp1>`

### 修复scope问题

上面的”out of context“意味着`temp1`解析到了环境中没有的binding。

哪些binding从环境中消失了？是first class intdef-ctx，local-expand结束后，其binding即不存在于环境了，但由于”全局表“的存在，仍能被解析到。既然出现了这个错误，那就表明结果中`temp1`的定义比ctx里的少了scope，或者`temp1`的定义比使用多了scope。

1. ctx中的binding多了其 _inside-edge scope_ ，即intdef scope，可以用`internal-definition-context-introduce`添加。

   ```racket
       (define id
         (internal-definition-context-introduce
          ctx
          (car (generate-temporaries (list expr)))))
   ```

   仍旧是`identifier used out of context: #<syntax temp1>`。

2. 通过Macro Stepper可以发现：`temp1`的定义比使用多了macro scope，`temp1`的定义所用的 _identifier_ 是`lift`直接传送给`local-expand/capture`的，所以这个scope是只能是`capme`展开结束后加入的。

   既然在结果中，`temp1`的定义和使用都是宏引入的，为什么只有定义带上了macro scope呢？

   因为`local-expand`。`local-expand`前后也会反转当前的macro scope和use-site scope，所以`temp1`的使用最终不带有`capme`的macro scope，而只有`liftme`的macro scope。

   反转定义中的scope：

   ```racket
       (with-syntax ([([id expr] ...) (reverse (unbox b))]
                     [expanded expanded])
         (with-syntax ([(id ...) (syntax-local-introduce #'(id ...))])
           #'(let ()
               (define id expr)
               ...
               (let ()
                 expanded))))
   ```

   得到`temp1: identifier's binding is ambiguous`。

3. 在DrRacket中点开错误信息，看到类似如下的binding信息：

   ```
   temp1: identifier's binding is ambiguous
     context...:
      #(67091 macro) #(67097 local) #(67098 intdef) #(67107 local)
      #(67108 intdef) [common scopes]
     matching binding...:
      local
      #(67091 macro) [common scopes]
     matching binding...:
      local
      #(67097 local) #(67098 intdef) [common scopes]
     common scopes...:
      #(67088 intdef) #(67092 macro) in: temp1
   ```

   不难看出第一个binding是ctx中的binding，第二个是结果中`temp1`的定义。也就是说ctx的binding也带有`liftme`的macro scope。

   这是由于`syntax-local-bind-syntaxes`也会反转当前的macro scope，所以：

   ```racket
       (syntax-local-bind-syntaxes
        (list (syntax-local-introduce id)) #f ctx)
   ```


通过上面的修改，这个程序输出`6`。

最终代码如下：

```racket
#lang racket/base
(require (for-template racket/base)
         racket/match)
  
(provide (all-defined-out))
  
(define current-lift-context
  (make-parameter #f))

(define (lift expr)
  (match-define (cons ctx b) (current-lift-context))
  (define id
    (internal-definition-context-introduce
     ctx
     (car (generate-temporaries (list expr)))))
  (syntax-local-bind-syntaxes
   (list (syntax-local-introduce id)) #f ctx)
  (set-box! b
            (cons (list id expr)
                  (unbox b)))
  id)

(define (local-expand/capture stx [intdef-ctx '()])
  (define ctx (syntax-local-make-definition-context))
  (define b (box '()))
  (define expanded
    (parameterize ([current-lift-context (cons ctx b)])
      (local-expand stx 'expression null (cons ctx intdef-ctx))))
    
  (with-syntax ([([id expr] ...) (reverse (unbox b))]
                [expanded expanded])
    (with-syntax ([(id ...) (syntax-local-introduce #'(id ...))])
      #'(let ()
          (define id expr)
          ...
          (let ()
            expanded)))))
```

