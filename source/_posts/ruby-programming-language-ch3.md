title: ruby programming language(ch3)
date: 2014-11-26 13:30:56
categories: lang
tags: [ruby, note]
---

# Ch3

## number

```
Numeric -- Integer
    |         |-- Fixnum
    |         `-- Bignum
    |----- Float
    |----- Complex*
    |----- BigDecimal*
    `----- Rational*
(* as standard lib of ruby, rest built-in)
```


``` ruby
1_000_000 == 1000000
-7%3 == 2 # in ruby, sign comply with 2nd operand; C,Java etc. the 1st. 
-a/b == b/-a != -(a/b) # ruby round to negative inf; C,Java not.
```

## text

### single-quote

* `\` escape only `\` and `'`, thus `a\b` == `a\\b`
* `'.. \<NL> ..'` has two lines, `\` **NOT** escape `\n`
* `'..'\<NL>'..'` concat lines, no introducing `\n`

### double-quote

* escape char
  - `\u{code1 code2 ..}` escape multi unicode char at one time **WITHOUT** space
* contnue line as single-quote

### generalized quoting

* `%q(.. no need escape here ..)`, `()` can be any matched `[]`, `{}`, `<>`, if not well-matched, **NEED** escape the delim
* `%Q` the same
* `%q_.. escape _ with \_ .._`, in case close delim (here `_`, others `!`, `-` also work) the same with open one

### here doc

`<<[end delim](e.g. EOF, __HERE__)`, no space in between `<<` and `[end delim]`

``` ruby
s = <<HERE + <<THERE + "end"
here
HERE
there
THERE
# s = "here\nthere\nend"
```

* `<<-[end delim]` allow spaces before `[end delim]`
* allow spaces in `[end delim]`, e.g. `# # #`

### shell command:

* `` `<shell-cmd>` `` replace with command output
* `%x<shell-cmd>` , either

example:

``` ruby
listcmd = win ? 'dir':'ls'
listing = `#{listcmd}`
# or
listing = Kernel.`(listcmd)
```

### single char

* `?<ch>`, `<ch>` can be any ascii code or unicode `\uxxx` or oct or hex or `\C-x`(ctrl-x, other similarly)

### operation on string

* `<<` to append, `*` to repeat
* `[]` index the exact unicode char **NOT** the codes in bytes
  - `[] = ?x` change with single char
  - `[] = ".."` with string
  - `[]` can be any integral pos(positive/negative), range, or regex
* `<string>.size == <string>.length ?= <string>.bytesize(care unicode)`

## array

* `%w` like `%q` create array literal in **STRING**
* `<array>.size == <array>.length`

### operation

* `+`: concat **BOTH** array
* `-`: set diff
* `<<`: append
* `*`: repeat
* `|`: union (order kept)
* `&`: intersect (order kept)
* `[]`: access, slice, assign as string

## hash

simple symbol style representation:

`numbers = { one: 1, two: 2, three: 3}`

complex:

`numbers = {:one => 1, :two => 2, :three => 3}` **DIFF** with `numbers = {"one" => 1, "two" => 2, "three" => 3}`

* hash keys compare with `eql?`(`==` in most cases)
* if `eql?` not provided, **object_id** is used via `hash` method; if `hash` not provided, that cannot be a hash key
* hashcode of mutable object changes when its content changes, thus hashes corrupt; or `rehash` to rescue
* `sring` is a special case handled though it's mutable

## range

**\!=** array

* literals
  - `a..b`: \[a,b\]
  - `a..b`: \[a, b\)
* variables:
  - `x..x*2`

only discrete range is iteratable, otherwise use `range.step() {..}`

conv:

* `r.to_s`, `r.to_a`
* care the trap: `1..3.to_a`

### membership

* `.member?` == `.include?`
* `.cover?` check if fit in the (dict-)sorted list from range_begin to range_end, so it **ALWAYS** use continous test, while `.member?` and `.include?` use continous when range ends are numbers, or fall to discreate when not numeric:
  - `triples = "AAA".."ZZZ"`
  - `triples.include? "ABC"`: true and slow
  - `triples.include? "ABCD"`: false and slow
  - `triples.cover? "ABCD"`: true and fast

### flip-flop (@Ch4)

take an example:

``` ruby
DATA.each do |line|
  if line =~ /start/ .. line =~ /end/
    puts line
  end
end

# prints line between "start" and "end"(include), so flip-flop works with STATE memory
```

## symbol

equal representation:

* `:sym`
* `:"sym"`, quote also can be used as `:"s y m"` to include spaces
* `:"#{s}" # s = "sym"`
* `%s[sym]`

conv:

* to string: `.to_s`, `.id2name`
* from_string: `.to_sym`, `.intern`

reflective:

``` ruby
if o.respond_to? :size
  o.send name
end
```

## object

### true/false

* only `nil` and `false` give **LOGICAL** false if `if`, while `0`, `""` give true
* `a && b` gives:
  - `a` if a is `nil` or `false`
  - `b` otherwise
* `=` is prior to `and`, `or`, so `if a = foo and b = bar` is ok, while not work with `&&`, `||`

### type

* `x.class`, `instance_of?` exact class, **NO** inheritance check
* `is_a?`, `kind_of?` works with inheritance and mixin

### equality

* `equal?` check exact object reference(object_id)
* `==` content identical, has casting, usually override
  - array: # same, each item compare with `==`
  - hash: # of k-v same, key compare with `eql?`, value with `==`
* `eql?` strict version of `==`, no casting, `eql?` requires `hash` method return same value, so must rewrite `hash` when define `eql?`
* `===` _case_ equality
  - most classes define `===` using `==`
  - `(1..10) === 5` true
  - `/\d+/ === 123` true
  - `String === "s"` true
* `=~` defined by **String** and **Regexp**, **Object** always return false; `!~` as inverse
* `<=>` if less, -1; elif equal, 0; else 1

### cast

on **coerce**:

take example:

1. `1.5 * Rational(1,3)`, equals to `(1.5).*(Rational(1,3))`, Float * Rational
2. invoke `Rational(1,3).coerce(1.5)`, since `1.5` does not know about `Rational(1,3)`
3. result as array `[Rational(1.5), Rational(1,3)]` where `1.5` get to Rational
4. then invoke `*`
5. useful when customizing class
