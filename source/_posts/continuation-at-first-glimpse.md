title: Continuation at First Glimpse
date: 2017-06-18 13:06:45
categories: lang
tags: [scheme]
---

继续tyss notes前先简单整理一下continuation的概念(偷懒把以前的笔记直接贴上来, 大概有不太准确的地方, 但应该不影响理解)

### 基本概念

continuation的中文一般都叫做"续延"(还蛮好听的), 不过解释起来比较麻烦, 还是结合代码来看更好理解一点.

首先scheme里的continuation(太长了, 下文缩写为cont吧)的基本形式是长这个样子的: 

```scheme
(call/cc
  (lambda (cc)
    ; do some computation)
)
```

`call/cc`是`call-with-current-continuation`的缩写, lambda的参数`cc`就是当前环境的一个cont, 可以理解为当前的调用栈, 它知道自己**未来**需要执行的过程. 那么怎么表示"未来的计算"这样一个概念呢? 在编程语言中自然就是函数了, 而在scheme中就是过程(procedure), 更一般的就用lambda来表示. `cc`就是这样一个只接受一个参数的lambda, 而调用`(cc val)`则是整个cont动作的关键----它将流程**跳**回cont定义的位置, 并以`val`代换为其计算出的结果.

来暴力理解一下, 把`(call/cc ..) (rest code..)`看做这样一个结构`[] (rest code ..)`, 在`(cc val)`的时候直接回到了`[]`的地方, 并将其替换为了`val`.

看一个简单(似乎不是特别简单)的例子:

```scheme
(+ 1 (call/cc)
      (lambda (cc) (+ 2 (cc 3)))
)
```

一步步来:

1. 整个block可以看做`(+ 1 [])`
2. 再看`[]`内部: `(lambda (cc) (+ 2 (cc 3)))`, 这个lambda的body是会被执行的
3. `(cc 3)`以3为参数调用cont, 则跳转到其定义的地方`[]`将其替换
4. 最后变为`(+ 1 3)`, 结果为4
5. `(+ 2 ..)`的部分并没有luan3用 :P

### 基本用法

#### Jump out

这个用法有个名字叫"非本地退出(non-local exit)", 说白了就是现代编程语言里break啦, exit啦这些东西.

* scheme没有这些关键字
* scheme觉得它们不够old-school
* scheme决定这么搞

```scheme
(define (search wanted? lst)
  (call/cc
    (lambda (return)
      (for-each (lambda (e)
                  (if (wanted? e) (return e)))
                lst)
      #f
    )
  )
)
```

功能很直白, 从一个list里找到符合条件的元素, 不存在就返回`#f`. `return`的用法是直接从`for-each`的循环中跳出了. 这里最后的`#f`可不可以写成`(return #f)`? 当然可以, 不过整个lambda都计算完了, 自然是返回最后一个计算值, 所以可以直接省略啦.

另一个例子

```scheme
(define (list-product list)
  (call/cc
    (lambda (exit)
      (let iter ((rest lst))
        (cond
          ((null? rest) 1)
          ((zero? (car rest)) (exit 0))
          (else (* (car rest) (iter (cdr rest))))
        )
      )
    )
  )
)
```

对一个list完成fold计算乘积的操作, 中途如果遇到0则立即退出, 避免了不必要的遍历.

#### Jump back

在scheme里, cont和lambda一样都是一等公民, 它自然也可以被当做参数抛来抛去, 或者跟其他变量进行绑定.

```scheme
(define return #f)
(+ 1 (call/cc
      (lambda (cont) (set! return cont) 1)
     )
)
```

老样子一步一步看:

1. 全局定义一个`return`(绑定啥值无所谓)
2. 替换cont形式: `(+ 1 [])`
3. 在lambda body中, `return`与cont`[]`绑定!
4. 1作为返回值, 则`(+ 1 [])`结果为2

然而并没有结束, 此时`return`本身已经作为一个"+1器"存在了, 它代表的cont即`(+ 1 [])`, 每次调用`(return v)`便会得到`v + 1`.

#### Jump out and Jump back

看到这里, 可以感觉到cont是能作为程序流控制的手段的, 用来完成一些比较时髦的动作, 比如大家都喜欢的协程 :P

所以接下来看一个模拟协程计算的较复杂的例子:

```scheme
(define (hefty-computation do-other-stuff) ; 主要复杂计算
 (let loop ((n 5))
   (display "Hefty computation: ")(display n)(newline)
   (set! do-other-stuff (call/cc do-other-stuff)) ; (1)
   (display "Hefty computation (b)\n")
   (set! do-other-stuff (call/cc do-other-stuff)) ; (3)
   (display "Hefty computation (c)\n")
   (set! do-other-stuff (call/cc do-other-stuff))
   (if (> n 0) (loop (- n 1)))
 )
)

(define (superfluous-computation do-other-stuff) ; 次要计算
  (let loop ()
    (for-each (lambda (item)
                (display item)(newline)
                (set! do-other-stuff (call/cc do-other-stuff))) ; (2)
              '("Straight up." "Quarter after." "Half past."  "Quarter til.")
    )
    (loop) ; this trigger inf loop
  )
)

(hefty-computation superfluous-computation)
```

为了大家(~~我自己~~)能简单的看懂, 我们走的慢一些:

0. 分别定义了两个过程: 主要计算和次要计算; 以调用主要计算开始, 参数为次要计算本身
1. 开始主要计算, 进入loop(第一次), 打印"Hefty computation: 5"
2. (1)处`call/cc`以次要计算为参数, 结合后者定义, 则有(为了方便, 分别记两个两个过程的参数为do1和do2):
3. 主要计算产生第一个cont, `(set! do1 []-1)`
  1. 开始次要计算, 参数do2为上一步的`[]-1`, 进入loop(第一次), for-each打印列表中第一个"Straight up."
  2. 到达(2) `(call/cc do2)`, 此时次要计算产生一个cont, 记为`[]-2`, 同时完成do2也就是`[]-1`的计算, 流程回到了(1), 且将do1绑定到了`[]-2`
4. 从(1)后继续主要计算, 打印"Hefty computation (b)"
5. 主要计算(3)处产生第二个cont`[]-3`, 形式和上次一样; 由于参数do1实际上是`[]-2`, 则:
  1. 跳转到`[]-2`即(2)处继续执行, 此时`set!`将`[]-2`的返回值即`[]-3`绑定到do2
  2. for-each的第二个循环打印"Quarter after."
  3. 流程又到(2)处, `(call/cc []-3)`, 并产生当前的cont`[]-4`, 作为`[]-3`的参数; 执行`[]-3`的计算, 于是又回到了(3)处
6. do1绑定为`[]-4`, 且打印"Hefty computation (c)"
7. 接下来的过程类似上面, 即不断的在主要计算和次要计算之间跳来跳去(和协程的切换是一样的)

看上去似乎有点晕, 总结一下这个用法的关键在于:

`(call/cc cc)`实际上是将当前的cont作为参数cont`cc`的结果, 然后通过执行`cc`跳转到cont创建时的地方; 之后通过`set!`将之前的cont(即`cc`的结果)保存下来, 然后再次通过`(call/cc cont)`跳回去, 如此往复.

### 最后一个例子

```scheme
(let* ((yin  ((lambda (cc) (write-char #\@) cc) ; proc
              (call/cc (lambda (c) c))))        ; arg
       (yang ((lambda (cc) (write-char #\*) cc) ; proc
              (call/cc (lambda (c) c)))))       ; arg
  (yin yang)
)
```

这是网上流传很久的yin-yang puzzle, 程序会不停的交替打印"@\*@\*\*@\***@ ...". 这个例子大家可以尝试自己拿纸笔推一下(虽然过程特别的绕, 但是通过不断代换的笨办法还是可以一战的), 或者可以看一下[这个解答](http://stackoverflow.com/questions/2694679/how-does-the-yin-yang-puzzle-work/36513942#36513942)就比较好理解了, stay calm :P

----

ref:

* http://geek-zh.net/cps-and-call-cc/
* http://www.ituring.com.cn/article/53793
* http://community.schemewiki.org/?call-with-current-continuation
* http://stackoverflow.com/questions/2694679/how-does-the-yin-yang-puzzle-work/36513942#36513942
