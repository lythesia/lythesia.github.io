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


| `ed` method     | when                   | param                              | extra                     |
|-----------------|------------------------|------------------------------------|---------------------------|
| `inherited`     | new class defined      | new class object                   | inherited by subclass     |
| `included`      | module included        | class or module object included in |                           |
| `extended`      | object extended        | ..                                 |                           |
| `method_added`  | instance method        | :"new method name"                 |                           |
| `singleton_..`  | singleton/class method | :"singleton_method"                | self invoked when define  |

others:

* `removed/undefined`
* `singleton_removed/_undefined`

## tracing

* `__FILE__`
* `__LINE__`
* `SCRIPT_LINES__`: hash { filename: array of lines of source file } filled when `require` and `load`
  - include main file: `SCRIPT_LINES__ = {__FILE__ => File.readlines(__FILE__)}`
  - refer line of code: `SCRIPT_LINES__[__FILE__][__LINE__-1]`
* `trace_var(:var)`: when changed

## ObjectSpace

* `.each_object`
* `.id2ref` inverse to `.object_id`
* ..

## def method dynamically

### class_eval

directly interpolate identifiers into method body

```ruby
class Module
  private
  
  # alias attr_reader
  def readonly(*syms)
    return if syms.size == 0
    code = ""       # final code to eval
    syms.each {|s| code << "def #{s}; @#{s}; end\n"} # code for getter
    class_eval code # eval to define
  end
  
  # alias attr_accessor
  def readwrite(*syms)
    return if syms.size == 0
    code = ""
    syms.each do |s|
      code << "def #{s}; @#{s}; end\n"
      code << "def #{s}=(val); @#{s} = val; end\n"
    end
    class_eval code
  end
end
```

### define_method

rely on reflective methods

```ruby
class Module
  # init INSTANCE vars with default
  # e.g: attributes x:0, y:0
  def attributes(hash)
    hash.each_pair do |symbol, default|
      # symbol name for getter/setter
      getter = symbol
      setter = :"#{symbol}="
      # symbol name for member var
      variable = :"@#{symbol}"
      # define getter via reflective
      define_method getter do
        if instance_variable_defined? variable
          instance_variable_get variable
        else
          default
        end
      end
      # define setter
      define_method setter do |val|
        instance_variable_set variable,val
      end
    end
  end
  
  # init CLASS vars
  def class_attr(hash)
    eigenclass = class << self; self; end
    eigenclass.class_eval { attributes(hash) }
  end
  
  private :attributes, :class_attrs
end
```
## DSL

> A DSL is just an extension of Ruby syntax (with methods that look like keywords)
> or API that allows to solve problem or represent data more naturally.

XML as example utilized ruby's:

1. block
2. parentheses-optinal invoke
3. passing hash literals without curly braces
4. `method_missing`

```ruby
class XMLGrammar
  def initialize(out)
    @out = out
  end
  
  # output as CDATA
  def content(text)
    @out << text.to_s
    nil
  end
  
  def comment(text)
    @out << "<!-- #{text} -->"
    nil
  end
  
  def tag(tagname, attributes={})
    # open tag
    @out << "<#{tagname}"
    
    # attr
    attributes.each {|attr,val| @out << " #{attr}='#{val}'" }
    
    # content
    if block_given?
      @out << ">"
      content = yield   # invoke block returned as content
      if content
        @out << content.to_s
      end
      # close pair
      @out << "</#{tagname}>"
    else
      # close single
      @out << "/>"
    end
    nil
  end
  
  # ordinary class ==> DSL
  # Sol 1: { any unknown method is treated as name of tag
  alias method_missing tag
  # }
  # run block in new instance of XML class
  def self.generate(out, &block)
    XML.new(out).instance_eval(&block)
  end
  
  # or
  # Sol2: { add validation without using `method_missing`
  
  # define an allowed element(tag) in one (XML) grammar
  def self.element(tagname, attributes={})
    @allowed_attributes ||= {}
    @allowed_attributes[tagname] = attributes
    
    class_eval %Q{
      def #{tagname}(attributes={}, &block)
        tag(:#{tagname}, attributes, &block)
      end
    }
    
    # constants indicate type of attribute
    OPT  = :opt   # optional
    REQ  = :req   # required
    BOOL = :bool  # value is own name
    
    def self.allowed_attributes
      @allowed_attributes
    end
    
    def tag(tagname, attributes={})
      @out << "<#{tagname}"
 
      # get allowed for current tag
      allowed = self.class.allowed_attributes[tagname]
      # check attribute exist
      attributes.each_pair do |k,v|
        raise "unknown attribute: #{k}" unless allowed.included? k
        @out << " #{k}='#{v}'"
      end
      
      # check attribute type
      allowed.each_pair do |k,v|
        # already output
        next if attributes.has_key? k
        if (v == REQ)
          raise "required attribute '#{k}' missing in <#{tagname}>"
        elsif v.is_a? String
          @out << " #{k} = '#{v}'"
        end
      end
      
      if block_given?
        # same as above
      end
    end
  end
  # }
end
```

usage example:

```ruby
# define
class HTMLForm < XMLGrammar
  element :form, :action => REQ,
          :method => "GET",
          :enctype => "application/x-www-form-urlencoded",
          :name => OPT
  element :input, :type => "text", :name => OPT, :value => OPT,
          :maxlength => OPT, :size => OPT, :src => OPT,
          :checked => BOOL, :disabled => BOOL, :readonly => BOOL
  element :textarea, :rows => REQ, :cols => REQ, :name => OPT,
          :disabled => BOOL, :readonly => BOOL
  element :button, :name => OPT, :value => OPT,
          :type => "submit", :disabled => OPT
end

# use
HTMLForm.generate(STDOUT) do
  comment "This is a simple HTML form"
  form :name => "registration",
       :action => "http://www.example.com/register.cgi" do
          content "Name:"
          input :name => "name"
          content "Address:"
          textarea :name => "address", :rows=>6, :cols=>40 do
            "Please enter your mailing address here"
          end
          button { "Submit" }
        end
end
```
