title: ruby programming language(ch4-5)
date: 2014-11-26 15:56:30
categories: lang
tags: [ruby, note]
---

# Ch4

## parallel assign

* `x,y,z = 1,2,3` same as `x,y,z = [1,2,3]` or `x,y,z=*[1,2,3]`(splat)
* swap: `x,y = y,x`(like python)
* lvalues can only have one splat:
  - `*x,y = 1,2,3`  [1,2],3
  - `x,y,*z = 1,*[2,3,4]` 1,2,[3,4]
  - `x,y,z = 1,[2,3]` 1,[2,3],nil, but `x,y,z=1,*[2,3]` 1,2,3
  - `*` on lvalue as compress, on rvalue as decompress

## flip-flop

* `(1..10).each {|x| print x if x==3..x>=3}` 3 because `..` does evaluation and righthand expr check(to flop out) at same time
* `(1..10).each {|x| print x if x==3...x>=3}` 34 because `...` waits evaluation first, then check and flop out

## oneline expr

* `(expr) if (expr)`
* (in python `(expr) if (expr) else (expr)` valid, while not in ruby)

# Ch5

## condition

### if modifier

``` ruby
begin
  line1
  line2
end if expr
```


``` ruby
( line1
  line2 ) if expr
```

### unless

* if-not
* no allow `elsif`

### case

* `case [when+] end` no need `break`
* can be used as expr value: `x = (case .. end)`

### while/until as modifier

* `expr while(until) expr`
* or as `if`

## iterate

* integral: `.upto`, `.downto`, `.times`
* continous: `start.step(end, step)`

* `.collect` (like `.map`?) traversal and process each
* `.select` out subset meet requirements, `.reject` as inverse
* `.inject` as reduce

### custom

* `yield` values to block as parameter, say invoke the block with yielded values
* `if block_given?` to determine whether invoke or not
* return as enumerator: `self.to_enum(:yout_yield_method)`, which can then invoke `.each`, etc.
* as external iterator: `loop {iterator.next}`, which raise **StopIteration**

## alter control flow

* `return`: allow return multi values from block, but jumps out of current enclosing method, thus return values as method values
* `break`: like `return` can have a value, but jumps out of whole iteration but not method scope
* `next`: allow return values, jumps out current iteration
* `redo`: restart current iteration, skipping checking the condition

## exception

### raise

* `raise "bad argument" if <cond>`
* `raise RuntimeError, "bad argument" if <cond>`
* `raise RuntimeError.new "bad argument" if <cond>`
* `raise RuntimeError.exception "bad argument" if <cond>`

### rescue

``` ruby
begin
  # code may raise exception
rescue => e # global var $! also handles it
  # handle it
else 
  # other uncatched
ensure
  # always run
end
```

variables in `rescue` scope are visiable to rest after

* `rescue Exception`
* `rescue ArgumentError => e`
* `rescue ArgumentError, TypeError => e` or-match

as modifier: `y = factorial(x) rescue 0`, handles any **StandardError** and return the default follows `rescue`

## BEGIN/END

* `BEGIN {..}` executes before anything else only once without considering conditions
* `END {..}` consider conditions; first register(encounter), last execute; only once even in loop


----


## thread

``` ruby
def readfiles(filenames)
  threads = filenames.map do |f|
    Thread.new { File.read(f) } # return value of block stores in thread.value, which is a sync method(will block if not complete)
  end
  threads.map {|t| t.value}
end
```

## fiber

lightweight thread

### normal

* `require 'fiber'`
* `.yield` transfers control back to caller, `.resume` to return

example:

``` ruby
f = Fiber.new {
  puts "fiber hel"  # 3.
  Fiber.yield       # 4. goto line 9
  puts "fiber bye"  # 7. goto line 11
}
  
puts "caller hel"   # 1.
f.resume            # 2. goto line 2
puts "caller bye"   # 5.
f.resume            # 6. goto line 4
# end
  
# results:
# caller hel
# fiber hel
# caller bye
# fiber bye
```

diff with `yield` in iteration:

* fiber yields to caller
* iter yields to block

### with arguments & return values

``` ruby
f = Fiber.new do |msg|
  puts "caller: #{msg}"
  msg2 = Fiber.yield "hel"
  puts "caller: #{msg2}"
  "fine"
end

response = f.resume "hel"
puts "fiber: #{response}"
response2 = f.resume "how are you" # "how are you" as the value of Fiber.yield
puts "fiber: #{response2}"

# results:
# caller: hel
# fiber: hel
# caller: how are you
# fiber: fine
```

### transfer

unlike thread, fibers must be explicitly scheduled via `transfer`

``` ruby
f = Fiber.new {|..| g.transfer(..)} # transfer contorl from f to g with arguments
g = Fiber.new {..}
```

## continuation

performs jump(goto)

take example:

``` ruby
$line = {} # global hash for store jump lables

def line(symbol)
  callcc {|c| $lines[symbol] = c} # make a continuation related to label
end

def goto(symbol)
  $lines[symbol].call # return to caller label
end

# now applying goto
i = 0
line 10 # 10 is a label can be anyone else
puts i += 1
goto 10 if i < 5 # jump

line 20 # same
puts i -= 1
goto 20 if i > 0 # jump
```

