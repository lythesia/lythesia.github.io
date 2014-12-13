title: ruby programming language(ch8)
date: 2014-12-11 21:26:58
categories: lang
tags: [ruby, note]
---

# Ch8

## type,class,module

### ancestry

```ruby
<module>.ancestors # => modules
<class>.ancestors  # => self, modules, superclass .. Object, Kernel(module)

<module>.included_modules # => ..
<class>.included_modules  # => .. Kernel
```

### def

```ruby
M = Module.new
D = Class.new(C) { include M }
```

## eval

### binding & eval

`eval 1 [2]`:

1. eval string
2. **Binding** object

```ruby
eval(.., obj.binding) # obj.binding.eval(..), make sure `binding` return the binding object
eval(..) # Kernel.eval(..) or eval(.., Kernel.binding)
```

### instance_eval & clas_eval

* code evaluated in **correspond** context (to instance or class object)
* both can accept **block** instead of string 

## reflect var

* `<instance>.instance_variables`
* `<class>.class_variables`
* `class.constants`

* query: `eval(".. #{..} ..")`
* attr: `o.instance_variable_set(:@x, 0)` `o.instance_variable_get(:@x)`, similar `class_xxx` `const_xxx`
* test: `_defined?` `_missing?`

## method

* `.methods` == `.public_methods`
* `.protected_methods` `.private_methods`
* `<class>.instance_methods` alternative prefix: `public` `protected` `private`
* `.singleton_methods`
  - trivial for instance
  - class methods for class
* `methods` gives list, with `(false)` to exclude inherited
* `method(:sym)` to get method object
  - `<method>.call ..`
  - `<object>.send(:sym, ..)`

### define

instance:

```ruby
def add_method(c, m, &b)
  c.class_eval {
    # private, must be inside class/module
    # `.class_eval` provides this context
    define_method(m, &b)
  }
end

add_method(String, :greet) { "Hello, " + self }
"world".greet # => "Hello, world"
```

class:

```ruby
def add_class_method(c, m, &b)
  eigenclass = class << c; self; end
  eigenclass.class_eval {
    define_method(m, &b)
  }
end

add_class_method(..)
String.greet("world") # => "Hello, world"
```

or 

```ruby
String.define_singleton_method(:greet) {..}
```

eigenclass gets the class object, recall a specified class is a instance

## hook

when:

* classes are subclassed
* modules are included
* methods are defined
* ..


