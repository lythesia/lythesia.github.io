title: ruby programming language(ch6)
tags:
  - ruby
  - note
categories: lang
date: 2014-11-28 21:16:28
---

# Ch6

## method

### singleton method

```ruby
o = "some object"
def o.echo
  puts self
end
o.echo # invoke
```

### alias

`alias <new> <origin>`

take an example:

```ruby
def foo
  # ..
end

alias real_foo foo

def foo
  # ..
  real_foo # it's ok
end
```

### default argument

* default arguments must be **adjacent**
* left-to-right assign

### block as argument

```ruby
def foo(.., &b)
  # ..
  b.call(..) # &b get to a Proc object, this replaces yield
end
```

* block argument and real block **cannot** be given at same time

### proc as argument

```ruby
p = Proc.new {..}
def foo(.., p) end
def bar(.., &b) end

# invoke
foo(.., p)
foo(.., &p)
```

### using &

* method symbol has `.to_proc` to Proc object
* `&` before Proc object make it applicapable by iterator, `lst.map &:upcase`
* `&:sym` = `&:sym.to_proc` in invocation

## Proc & Lambda

* both `is-a` **Proc**
* == only via `dup,clone`

### create proc 

from block:

```ruby
def makeproc(&p)
  p
end
```

or:

* `Proc.new`
* lambda
  - `succ = lambda {|x| x+1}`
  - `succ = ->(x){ x+1 }`
  - `f = ->(x,y; i,j,k) {..}` or `f = -> x,y; i,j,k {..}` arg: x,y; local-var: i,j,k (minial lambda: `->{}`)

### invoke

for both proc and lambda:

* `f.call (..)`
* `f[..]`
* `f.(..)`

### diff

#### return

proc:

```ruby
def makeproc(msg)
  p = Proc.new { puts msg; return } # return to makeproc
  puts "return from proc"
  p   # but already return to caller(here test)
end

def test
  puts "enter method"
  p = makeproc "enter proc"
  p.call
  puts "exit method"  # so never executed
end

test
# enter method
# return from proc
# enter proc
# >> LocalJumpError
```

lambda:

```ruby
def makelambda(msg) .. end

def test .. end

test
# enter method
# return from lambda
# enter lambda
# exit method
# (no LocalJumpError)
```

#### break

* proc: as block break out whole iterator
* lambda: normal break, only make sense within enclosing loop or iteration

#### arguments

* proc: no number match, auto pack/unpack, life easier
* lambda: exact match, no auto splat

## closure

* proc and lambda are **closures**
* closure possess the **copy** of local context when **created**
* alter via binding: 
  1. `def foo(n); lambda {..}; end`
  2. then `f2 = foo(2)` 
  3. f2 can altered to `eval("n=3", f2)` or `eval("n=3", f2.binding)` (complete form)

## method object

```ruby
m = 0.method :succ

m.call or m[]
```

**not** closure

unbound method object:

```ruby
unbound_plus = Fixnum.instance_method "+"
plus_2 = unbound_plus.bind 2
plus_2[2] # => 4
plus_3 = plus_2.unbound.bind 3
```

## functional

### enumerable as example

```ruby
module Functional
  # some_method | enumerable: apply some_method to enumerable as map
  def apply(enum)
    enum.map &self
  end
  alias | apply

  # some_method <= enumerable: .. as reduce
  def reduce(enum)
    enum.inject &self
  end
  alias <= reduce
end

# mixin to Proc & Method
class Proc; include Functional; end
class Method; include Functional; end

# take standard deriv as example
stddev = -> a { 
  sum = -> x,y { x+y }
  mean = (sum <= a) / a.size # if you want to make it lambda, should ensure it can refer `a` in invoking deriv
  deriv = -> x { x - mean }
  square = -> x { x*x }
  Math.sqrt( (sum <= square|(deriv|a)) / (a.size - 1) ) 
}

stddev[[1,2,3,4]]
# => 1.2909944487358056 
```

### compose

```ruby
def compose f
  if self.respond_to?(:arity) && self.arity == 1
    -> {|*args| self[f[*args]]}
  else
    -> {|*args| self[*f[*args]]} # handles more than one args, if self is defined like that
  end
end
alias * compose
```

### partial apply

```ruby
def apply_head(*first); -> *rest { self[*first.concat rest] }; end
def apply_tail(*last); -> *rest { self[*rest.concat last] }; end
alias >> apply_head
alias << apply_tail

g = f >> 2 # set 1st arg to 2
g = f << 2 # set last arg to 2
```

### memorize

```ruby
def memoize
  cache = {} # An empty cache. The lambda captures this in its closure.
  lambda {|*args|
      # notice that the hash key is the entire array of arguments!
    unless cache.has_key?(args) # If no cached result for these args
      cache[args] = self[*args] # Compute and cache the result
    end
    cache[args] # Return result from cache
  }
end
# A (probably unnecessary) unary + operator for memoization
# Mnemonic: the + operator means "improved"
alias +@ memoize # cached_f = +f @ for prefix unary
```

example: 

```ruby
# recursive
factorial = ->x {..}.memorize or +lambda {|x|..} # ok, cached
factorial = ->x {..}; cached_f = +factorial      # not cached, only you called each x once, that x is cahced!
```

### with symbol

```ruby
# ..
alias [] bind
String[:reverse]["hello"][] # => "olleh"
#   unbound     bind-to  invoke
#   method      "hello"
```

