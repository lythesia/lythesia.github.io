title: ruby programming language(ch1-2)
date: 2014-11-25 22:11:55
categories: lang
tags: [ruby, note]
---

# Pre-study

## rvm

If you need to separate configurations & gems for each ruby project individually, [rvm](rvm.io) is a good tool.

install check:

* `type -t rvm` gives `function`

usage notes:

* `rvm install ruby <version>`
* `rvm use <version> --default` to set default `ruby` to use
* `rvm gemset use <group>` and then `gem install <gem>` to install gem to that `group`
  - gems installed to `global` are inheritable

some tips:

* make sure your term enable **login-shell**
* source rvm at last as possible to avoid warning: `...path/to/ruby-bin not at first place in PATH...`

## pry

Interactive tool like `bpython`, i use `pry --simple-prompt` for convenient like noob test, etc.

## ri

Ruby manual, e.g: `ri Array`, `ri Array.sort`, `ri Hash#each`, `ri Math::sqrt`.


----


# Ch2

## comments

```ruby
# line comment
```

```ruby
=begin
block comment
=end
```

* there's no `/* */` like comment.
* `#` is prior to `=begin(end)`

## some symbol in identifiers

prefix/suffix:

* `$`: global var
* `@`: class instance var
* `@@`: class var
* `xx?`: boolean-valued method
* `xx!`: in-place method
* `xx=`: method invoked by assignment

reserved word:

* `__LINE__`
* `__ENCODING__`
* `__FILE__`
* `__END__`: after goes the data, you can refer these data by `DATA`

## spaces

one line is implicitly closed if it's a **complete** expression, check:

```ruby
total = x +
y
```

```ruby
total = x
+ y
```

after ruby1.9, newline start with `.` implicitly considered as continuel line, like chain-invoke:

```ruby
ani = Array.new
  .push("doge")
  .push("nya")
  .sort
```

ruby has grammar candy for method invoke, care the trap:

```ruby
f(3+2)+1
f (3+2)+1
```

use `ruby -w` to check ambiguous warning.

