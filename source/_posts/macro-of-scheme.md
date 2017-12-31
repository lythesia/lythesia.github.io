title: macro of scheme
date: 2017-12-31 10:28:28
categories: lang
tags: [scheme]
---

### intro

大家对宏(特指C/C++的宏)应该不陌生, 甚至被这个东西坑过无数次, 究其原因, C/C++的宏本质上只是文本替换而已, 没有任何语法层面的信息, 因而也就做不到类型检查语法检查这些事情; 而scheme的宏是真正工作在语法树上的, 甚至可以对其进行修改!

### define-syntax

在R5RS以前, scheme通过`define-macro`来定义宏, 形式如下:

```scheme
(define-macro (..)
  ; usually
  `(..
    ,(eval-to-single-var)
    ,@(eval-to-list)
  )
  ; so use it as **template**!
)
```

行为上其实和C/C++的宏是差不多的, 在transform阶段简单的做了token的替换, 看一个简单的例子:

```scheme
(define-macro
  myor
  (lambda (a b)
    `(let ([tmp ,a])
      (if tmp #t ,b)
    )
  )
)
(let ([tmp #t]) (myor #f tmp)) ; oops, here gives #f, not expected
```

这个坑很明显, 和C/C++一样, 宏内部出现的名字`tmp`被实际使用宏的代码污染了. 不过聪明的scheme实现会提供一个`gensym`方法, 用来生成一个独一无二的名字, 好我们试试看改进版本:

```scheme
(define-macro
  myor
  (lambda (a b)
    (let ([t (gensym)])
      `(let ([,t ,a]) ; what if `a` used variable named `t`? it's ok, `a` is lexical scoping
         (if ,t ,t ,b)
      )
    )
  )
)
```

虽然通过`gensym`解决了临时变量的名字问题, 但是依然没办法阻止用户污染其他名字, 比如重新定义`if`..于是我们意识到:
1. 名字(binding)在上下文环境里的重要性
2. 宏**被定义**时的环境和**被执行**的环境是两个完全不同的概念

回头来看`define-macro`的行为, 其实它的工作只是**替换**代码, 本身并不携带任何binding信息, 或者说, 它的binding信息来源于真正被执行的环境, 这个环境我们叫做**动态作用域**

那么, 显然在这种情况下, 我们需要一种能够拥有自己独立binding环境的宏. scheme在R5RS后提供了新的语法


### define-syntax

从名字可以感觉出来, 这种宏应当是工作在语法层面, 形式大致如下

```scheme
(define-syntax name
  (syntax-rules ( reserved words )
    ((_ arg) .. deal with arg ..)
  )
)
```

* `_`表示macro本身的placeholder
* `syntax-rules`则支持模式匹配

#### syntax-rules

模式匹配是一个很强大的功能, 可以很方便的定义一些含有递归结构的逻辑(scala中的pattern matching甚至能够一定程度上实现语义的匹配, 用起来也是非常顺手)

一个简单的例子:

```scheme
; (let*
;  ((..) (..))
;  body
;  )
(define-syntax let*
  (syntax-rules
    ()
    ((_ ((p v)) b ...)
     (let ((p v)) b ...)
     )
    ((_ ((p1 v1) (p2 v2) ...) b ...)
     (let ((p1 v1)))
      (let* ((p2 v2) ...) b ...)
     )
  )
)
```

可以看到`let*`的定义非常简单, 甚至可以像普通方法一样进行递归定义

#### hygienic

读者可以尝试一下在`let*`中进行各种"名字污染"行为(比如重新定义`let`), 结果当然是可以正确执行. 因为`define-syntax`引入了hygiene macro(卫生宏)的概念, 即: 宏内部使用的binding信息来源于被定义时的环境, 而不受到运行环境的影响, 这也叫作referential transparency. 对应的, 我们称hygiene macro工作在**词法作用域**

这是不是意味有了卫生宏的特性我们不需要动态作用域的功能了呢? 也不尽然, 看一个场景

```scheme
(define-macro
  show-vars
  (lambda (. vars)
    `(begin
       (display
         (list
           ; question about ',x and ,x?
           ; remember we are generating code here!
           ; ,x  -> gives the name, just like we input `x` in repl, and `x` will be evaluated as variable
           ; ',x -> gives `'some-symbol`, so it is evaluated as symbol!
           ,@(map (lambda (x) `(list ',x ,x)) vars))
       )
       (newline)
    )
  )
)
(let ([i 1] [j 3]) (show-vars i j)) ; gives ((i 1) (j 3))
```

`show-vars`展示了当前环境下定义的变量的**名字**和内容, 而这是一个运行时的环境, 恰好`define-macro`能够做到, 这也是词法作用域和动态作用域的区别

#### syntax-case

scheme也提供了比`syntax-rules`更细粒度的语法控制能力(为什么这么说?), 其形式如下:

```scheme
(define-syntax some-macro
  (lambda (syntax-form)
    (syntax-case syntax-form ()
      [(_ pattern ...)
       <fender>
       <expr> ...
      ]
      ...
    )
  )
)
```

关于`fender`的概念(不过这里没有用到):

> If the optional <fender> is present, it serves as an additional constraint on acceptance of a clause. If the <pattern> of a given <syntax-case clause> matches the input value, the corresponding <fender> is evaluated. If <fender> evaluates to a true value, the clause is accepted; otherwise, the clause is rejected as if the pattern had failed to match the value. Fenders are logically a part of the matching process, i.e., they specify additional matching constraints beyond the basic structure of the input.

对比一下两者的特点:

```scheme
(define-syntax when
  (syntax-rules ()
    ((_ test e e* ...)
     (if test (begin e e* ...)))))

(define-syntax when
  (lambda (x)
    (syntax-case x ()
      ((_ test e e* ...)
       #'(if test (begin e e* ...))))))
```

* 都支持pattern matching
* `syntax-case`的返回有`#'`前缀: 实际上被用来替换在pattern matching里被捕获的模式变量
* (语法上看不出来的)`syntax-case`提供了**拆解**和**重组**语法对象的能力, 即操作syntax-object的能力(什么是syntax-object? 我们先往下看)

#### datum & syntax object

比如我们想实现这样一个宏`aif`:

`(aif (getuid) (display it) (display "none"))`, `it`是一个动态的binding, 显然aif需要工作在动态作用域

##### 版本1

```scheme
;; doesn't work
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((_ test then else)
       #'(let ((it test))
           (if it then else))))))
```

`then` `else` 都是syntax-object, 在syntax-form中作为模板变量被替换时仅仅保留了各自的**词法上下文**(lexical scope), 因此它们都不能访问`it` (因为在他们定义的环境中并没有`it`, 这也是referential transparency的体现)

##### 版本2

```scheme
;; doesn't work either
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((_ test then else)
       (let ((it (datum->syntax x 'it))) `it`(1)
         #'(let ((it test)) ; this `it`(2)
             (if it then else))))))) ; and this `it`(2), both not references `it`(1) (there are diffrerent objects! or sth)
```

* `datum->syntax`用于把一个symbol变成给定syntax-form中的syntax-object
* `#'`内部只会替换在pattern match里被捕获的模式变量, 其他的名字则引用自定义该macro时的词法上下文

虽然通过`datum->syntax`引入了一层lexical scope, 但是请注意该scope是相对于`x`(即整个`(aif ..)`调用的syntax-object)来说的, 换句话说是`aif`调用的上下文(例如`(let ([..]) (aif ..))`, 则aif的上下文即let以及let的外层环境); 而在`#'(let ((it test)) ..)'`中的`it`仅仅是展开后的一个名字, 与`datum->syntax`引入的`it`并不是同一个东西, 尽管后者确实能被then/else引用到(如果有被定义的话, 而在这个例子里, 尽管它拥有可以被then/else访问的lexical scope, 但实际上环境里并没有定义它, 因而会出现unbound variable错误)
(hint: 在drracket里可以很方便的看到referencing的情况)

##### 版本3

```scheme
;; works, but is obtuse
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((_ test then else)
       ;; invoking syntax-case on the generated
       ;; syntax object to expose it to `syntax'
       (syntax-case (datum->syntax x 'it) ()
         ; following `it` is not relevant with (datum->syntax ..), so you can name it `yy` sth..
         (it
           #'(let ((it test))
               (if it then else))))))))
```

引入一层定义`it`的环境, 然后再通过`syntax-case`来捕捉到这个模式变量, 此时then/else所处的环境是在`it`被引入且定义的环境中:

为了方便说明, 使用yy替换

```
(yy                       ; yy 作为模式变量, 捕获了 (datum->syntax x 'it), 这个syntax-object在将要发生的调用中的名字就是it
  #'(let ((yy test))      ; expansion后实际上变成了 ((it <test所代表的表达式>))
      (if yy then else))) ; 同理这里的yy也变成了it, 而此时it在let的环境中, 可以被then/else所引用
```

所以实际上, 我们是在then/else调用时能访问到的环境中引入了it这个名字(并通过let定义); 假设我们不是通过`syntax-case`来捕获到yy, 那么在模板中yy仅仅是一个名字(就像我们在repl里直接输入yy), 显然在then/else的lexical socpe里当然引用不到yy.
由此可以知道syntax-object是一个存在于某个上下文环境(**有意义**)的名字!

从`aif`的例子也可以看到, `define-syntax`也可以具有动态作用域的能力, 实际上我们想一下scheme里面为什么把`syntax-rules/case`叫做**transformer**? 因为它们能够:

1. 通过pattern matching来捕捉syntax-object(是不是像在分析语法树?)
2. 捕捉到的syntax-object可以被直接eval, 也可以被再次拆解/修改/引入新的syntax-object, 并且它们都属于当前操作的syntax-form的binding(是不是感觉像在编辑语法树?)
3. 没有被捕捉到的对象则保持定义时的binding, 从而避免了污染问题
