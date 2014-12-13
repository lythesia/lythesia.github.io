title: ruby programming language(ch7)
date: 2014-12-02 13:28:03
categories: lang
tags: [ruby, note]
---

# Ch7

## class def

last expression in def is the value of class def(`nil` if last is a method def)

### init

```ruby
class Point
  def initialize(x,y)
    @x,@y = x,y
  end
end
```

### attr

* `attr_reader :x, :y`
* `attr_accessor "x", "y"`

### duck type

loosen:

```ruby
def +(other)
  Point.new(@x + other.x, @y + other.y)
end
```

strict(for duck type):

```ruby
def +(other)
  raise TypeError, "Not Point-like" unless other.respond_to? :x and other.respond_to? :y
  Point.new(..)
end
```

### coerce

class operation cannot hanle like `other_class + self_class`:

```ruby
def coerce(other)
  # simply swap self and other, then apply self + other is ok
  [self, other]
end
```

### enum

```ruby
def each
  yield @x
  yield @y
end
```

thus utilize methods in `include Enumerable` based on `each`

### equality

equal:

```ruby
def ==(o)
  @x == o.x && @y == o.y
rescue
  false
end
```

identity:

```ruby
def eql?(o)
  if o.instance_of? Point
    @x.eql?(o.x) and @y.eql?(o.y)
  else
    false
  end
end

def hash
  # trick for hash code generation
end
```

### order

```ruby
include Comparable

def <=>(other)
  return nil unless other.instance_of? Point
  # compare strategy
end
```

### quick mutable

via `Struct`(core ruby class):

```ruby
Struct.new("Point", :x, :y) # Struct::Point
Point = Struct.new(:x, :y)
```

### class method

```ruby
def self.foo
end

# or
class << self
  def foo; ..; end
end
```

### var

constant:

* `FOO = Point.new(..)` (`initialize` must be defined before)
* `Point::BAR = ..` outside define is ok

class var:

* `@@` init in class def
* available in both instance & class method

class instance var:

* `@` init in class def
* only available in class method
* `class << self; attr_accessor :..; end`

## method visibility

```ruby
class Point
  # public goes here

  protected
  # protected goes here
  # like c++

  private
  # private goes here
end
```

## inherit

tips:

* `class derive < base` only 'public' inherit
* `any_class.class # => Class` instance_of **Class**
* `any_class.superclass # => Object` subclass_of **Object**

### override

* `alias :super_inv :super_method_to_be_overrided` to call method of superclass
* only subclass when familiar with it (private methods can be occasionaly overrided!), or encapsulate/delegate if only to use public API
* class method also override, better to invoke it through the class which defines it

### var

* instance var
  - not inherited
  - create on assign(usually done by method invoke)
  - if same name, that value is **overwritten** not **shadowed**
* class instance var
  - not inherited
* class var
  - inherited, so alterable in subclass
  - better to use class instance var instead, to avoid the above case
* constant
  - bound to lexical scope within method(inherited method use constant of superclass)

## object create

* `clone` can keep the frozen state while `dup` not
* both use `initialize_copy` (consider copy-constructor)

``` ruby limit-creation
private_class_method :new,:allocate
private :dup,:clone
```

## module

A named group of methods, contants and class vars.

```ruby
module Base64
  def self.encode
  end

  def self.decode
  end
end
```

allow nesting with classes

### mixin

1. module defines bunch of method rely on some class instance methods(as interface)
2. `include module` in class
3. define the relied methods

* include module in module
* `object.extend(module)` apply on a object

### loading


|     term     |   require    | load           |
|--------------|--------------|----------------|
| allow binary | Y            | N              |
| path         | library name | exact filename |
| repeat       | once         | mutiple times  |
| $SAVE level  | 0            | current        |


### lazyload

```ruby
# require 'socket' if TCPSocket first used
autoload :TCPSocket, "socket"
```

## eigenclass

* anonymouse class attached to an object, to open it `class << some; def .. end end`
* `class << class_name` define class method of class_name or within class def `class << self`
* `class << object` define singleton method of object

## method lookup

take `o.m`, search sequently:

1. eigenclass of `o` for singleton method `m`
2. instance method `m`
3. modules (reverse the include order)
4. inheritance (repeat 2-3)
5. lookup `method_missing`

## constant lookup

1. lexical
2. inheritance: `.. -> Object(as top level) -> Kernel`

example:

```ruby
module Kernel
  # Constants defined in Kernel
  A = B = C = D = E = F = "defined in kernel"
end
# Top-level or "global" constants defined in Object
A = B = C = D = E = "defined at toplevel"
class Super
  # Constants defined in a superclass
  A = B = C = D = "defined in superclass"
end
module Included
  # Constants defined in an included module
  A = B = C = "defined in included module"
end
module Enclosing
  # Constants defined in an enclosing module
  A = B = "defined in enclosing module"
  class Local < Super
    include Included
    # Locally defined constant
    A = "defined locally"
    # The list of modules searched, in the order searched
    # [Enclosing::Local, Enclosing, Included, Super, Object, Kernel]
    search = (Module.nesting + self.ancestors + Object.ancestors).uniq
    puts A # => "defined locally"
    puts B # => "defined in enclosing module"
    puts C # => "defined in include module"
    puts D # => "defined in superclass"
    puts E # => "defined toplevel"
    puts F # => "defined kernel"
  end
end
```

