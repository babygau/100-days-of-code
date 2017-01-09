my goal
=======

a new year is just around the corner and it will be very significant to my life and my family. i'm feeling so excited yet intimidated to the challenges I set out in new year, and this **\#100daysofcode** is one of them that i want to complete.

however, i also want to emphasis that i'm no way a professional developer, i'm doing this purefly because it's my passion. therefore, i'm not looking into doing something that could give you any **wow** or **awe** moment.

here is what i hope to gain through the challenge

- step \#1: build my first ever blog, up and running

  i will use `hakyll` as a static blog generator. the reason will be explained in next step

  the ui will be pretty much a copy cat from default template, because i have zero experience with `haskell` for now

- step \#2: learning `haskell` through `haskell book`, i've been falling in love with this book since day one i bought it but haven't managed the time to learn it

  by now you would know that i will start the challenge with `haskell` because i'm so curious about functional programming language, about `monad` and everything in between. this will also help me equip knowledge to learn about `typescript` which is also a strong static typing language

- step \#3: re-evaluate my blog

  by the time i complete the book, i believe i will be confident enough to play with `hakyll` in deep, yet it's time to dust of my outdate `css3` skill.

  i will use `postcss` for my blog style

- step \#4: start learning trio `typescript+react+redux` through `egghead.io`
- step \#5 and final: build a website using everything i would have learned through challenge

my logs
=======

day \#3: 09/01/2017
-------------------

** what i have done?**

- [x] review some `prelude` commands/functions

  + quit `ghci`: `:q` or `:quit`
  + unload module: `:m`
  + load file: `:l` or `:load`
  + information: `:info` or `:i`
  + type of: `:t` or `:type`

- [x] chapter 4 of `haskell book` plus exercises
- [x] chapter 5 of `haskell book` plus exercises

** lesson learned?**

- i see some similarities between `haskell` data type and `typescript`

  + `data type` vs. `typescript union`
  + `type constructor`, `data constructor` vs. `typescript interface`
  + `typeclass` vs. `typescript interface implement`
  + `type variables` vs. `typescript generic`
  + both have _type alias_

- predefined datatypes: `Int`, `Double`, `"String"`, `(Tuple)`, `[List]`
- conditional flow control with `if-then-else`
- `typeclass` is a set of operations defined with respect to a generic/polymorphic type
- `data constructor` can either have one or more arguments or be a constant value (e.g `function type (->)`)
- `type constructor` is **not** value and can **only be used in** type signatures, and used to denote `type constructor`
- `function type (->)`

  + take arguments (`type variables`) **but** has no data constructor: `data (->) a b`
  + is right associative
  + is _curried_ by default (or _partial application_): many functions, but each only take 1 argument
  + could be _uncurried_ (one function, many arguments): `(a, a) -> a` instead of `a -> a -> a`

- type signatures may have three different types: _concrete_, _constrained_, and _parametric polymorphism_
- _polymorphism_

  + _constrained or ad-hoc polymorphism_: is defined in similar to `typeclass`: `(+) :: Num a => a -> a -> a`
  + _parametric polymorphism_: refer to type variables that are **fully** polymorphic, not constrained by `typeclass` and could be **anything**: `id :: a -> a`
- if a variable could be anything (_parametric polymorphic_), it has no method
- if it can be some types (_typeclass instance), it has some methods
- if it is a concrete type, it lose the flexibility but gain more potential method

day \#2: 08/01/2017
-------------------

**what i have done?**

- [x] `.pdf` file can be viewed in `emacs 25` for my windows 10, feel much more productive!
- [x] chapter 2 of `haskell book`
- [x] chapter 3 of `haskell book`

**lesson learned?**

- `haskell` is non-strict (lazy) evaluation, means that not everything will get reduce to its irreducible form immediately
- *redex*: a reducible expression
- *infix* and *prefix* operator

  + `haskell` function has prefix syntax as default
  + arithematic operators has infix syntax: `(+)`, `(-)`, `(*)`, `(/)`
  + user \`:info\` to find information whether an operator is infix and its associativity and precedence
  + the higher precedence (0-9) will be applied first
  + there are *left* and *right* associative

- function application operator `$`

  + is right associative
  + very useful when you want to reduce/eliminate pairs of parentheses
  + easier to read code syntax

- `haskell` use **indentation** to define block of code expressions
- *syntactic sugar* is syntax designed to make expression easier to read or write (ex. `let..in`, `where`)
- when you write code in a source file, the order is unimportant, but when writing code directly into `REPL`, the order does matter
- `main :: IO ()` is not only function but a series of instructions to execute, which can include applying functions and producing **side-effects** such as printing to the screen.
- top-level vs local definition

  + `let` and `when` introduce local scope

- type signature `::`
- `string` type and concatenation and list functions: `++`, `concat`, `take`, `drop`...

day \#1: 07/01/2017
-------------------

**what i have done?**

- [x] set up my workflow for the challenge

  + set up `emacs` workspace and packages
  + initiate the blog using `hakyll`

- [x] chapter 1 of `haskell book`

  + no coding task in this chapter
  + learned about lambda/abstraction function terms

    * lambda structure: head and body
    * *alpha equivalence*
    * *beta reduction*
    * free variable
    * multiple argument
    * *combinator*: a lambda with no free variables
    * *divergence* or *omega*: reduction process never terminates or end (infinite loop)

  + completed chapter 1 exercises

**lesson learned?**

- to tame `spacemacs` layout and workspace
- to have the foundation of `haskell` lambda/function.
