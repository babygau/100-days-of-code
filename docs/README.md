# my logs

## day \#8: 14/01/2017

**what i have done?**

- [x] reviewed previous chapters of `haskell book`, especially topic about _weak
  head normal form_ and _normal form_
- [x] watch egghead videos about improving website performance with `webpack 2`

**lesson learned?**

- in `haskell book`, the author defined _normal form_ and _weak head normal form
  (whnf)_ as following:

  > values in haskell get reduced to weak head normal form by default. by
  > ‘normal form’ we mean that the expression is fully evaluated. ‘weak head
  > normal form’ means the expression is only evaluated as far as is necessary
  > to reach a data constructor

  > _whnf_ is a larger set and contains both the possibility that the expression
  > is fully evaluated (_normal form_) and the possibility that the expression
  > has been evaluated to the point of arriving at a `data constructor` or
  > `lambda` awaiting an argument

  however, i found the explanation is quite difficult to make head or tail, the
  following explanation by [hammer@stackoverflow](http://bit.ly/2invB0P) seem easier to understand

  > _normal form_
  >
  > an expression in normal form is fully evaluated, and no sub-expression could be evaluated any further (i.e. it contains no un-evaluated thunks).
  >
  > these expressions are all in normal form:
  >
  > ```haskell
  > 42
  > (2, "hello")
  > \x -> (x + 1)
  > ```
  >
  > these expressions are not in normal form:
  >
  > ```haskell
  > 1 + 2                 -- we could evaluate this to 3
  > (\x -> x + 1) 2       -- we could apply the function
  > "he" ++ "llo"         -- we could apply the (++)
  > (1 + 1, 2 + 2)        -- we could evaluate 1 + 1 and 2 + 2
  > ```
  >
  > _weak head normal form_
  >
  > an expression in weak head normal form has been evaluated to the outermost data constructor or lambda abstraction (the head). sub-expressions may or may not have been evaluated. therefore, every normal form expression is also in weak head normal form, though the opposite does not hold in general.
  >
  > to determine whether an expression is in weak head normal form, we only have to look at the outermost part of the expression. if it's a data constructor or a lambda, it's in weak head normal form. if it's a function application, it's not.
  >
  > these expressions are in weak head normal form:
  >
  > ```haskell
  > (1 + 1, 2 + 2)       -- the outermost part is the data constructor (,)
  > \x -> 2 + 2          -- the outermost part is a lambda abstraction
  > 'h' : ("e" ++ "llo") -- the outermost part is the data constructor (:)
  > ```
  >
  > as mentioned, all the normal form expressions listed above are also in weak head normal form.
  >
  > these expressions are not in weak head normal form:
  >
  > ```haskell
  > 1 + 2                -- the outermost part here is an application of (+)
  > (\x -> x + 1) 2      -- the outermost part is an application of (\x -> x + 1)
  > "he" ++ "llo"        -- the outermost part is an application of (++)
  > ```
  > **stack overflows**
  >
  > evaluating an expression to weak head normal form may require that other
  > expressions be evaluated to whnf first. for example, to evaluate 1 + (2 + 3)
  > to _whnf_, we first have to evaluate  2 + 3. if evaluating a single
  > expression leads to too many of these nested evaluations, the result is a stack overflow.
  >
  > this happens when you build up a large expression that does not produce any
  > data constructors or lambdas until a large part of it has been evaluated.
  > these are often caused by this kind of usage of foldl:
  >
  > ```haskell
  > foldl (+) 0 [1, 2, 3, 4, 5, 6]
  > = foldl (+) (0 + 1) [2, 3, 4, 5, 6]
  > = foldl (+) ((0 + 1) + 2) [3, 4, 5, 6]
  > = foldl (+) (((0 + 1) + 2) + 3) [4, 5, 6]
  > = foldl (+) ((((0 + 1) + 2) + 3) + 4) [5, 6]
  > = foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) [6]
  > = foldl (+) ((((((0 + 1) + 2) + 3) + 4) + 5) + 6) []
  > = (((((0 + 1) + 2) + 3) + 4) + 5) + 6
  > = ((((1 + 2) + 3) + 4) + 5) + 6
  > = (((3 + 3) + 4) + 5) + 6
  > = ((6 + 4) + 5) + 6
  > = (10 + 5) + 6
  > = 15 + 6
  > = 21
  > ```
  > notice how it has to go quite deep before it can get the expression into weak head normal form.
  >
  > you may wonder, why does not haskell reduce the inner expressions ahead of time? that is because of haskell's laziness. since it cannot be assumed in general that every subexpression will be needed, expressions are evaluated from the outside in.

  to sum up: _whnf_ = _normal form_ \| λ(_normal form_) \| `data constructor`

## day \#7: 13/01/2017

**what i have done?**

- [x] going half way of chapter 9 of `haskell book`, some examples need to be
  reviewed carefully in order to understand more about recursive implementation.

- **note:** in the coming week, i'm gonna be so busy with works, that means the
  challenge progress would be slowed down, but i do hope, that's not gonna
  affect my goals.

**lesson learned?**

- list reducer with `foldl`, `foldr`, `scan`

  ```haskell
  -- foldr
  foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr f z xs =
    case xs of
      []     -> z
      (x:xs) -> f x (foldr f z xs)

  foldr (+) 0 [1, 2, 3]
  -- 6
  ```

  + there are 2 stages when folding a list, _traversal_ and _folding_

    _traversal_ is the stage which fold recursive over the _spine_

    _folding_ refers to the evaluation of the folding function applied to  the
    values

    give these two stages and _non-strict evaluation_, **if `f` doesn't evaluate
    its second argument (_rest of the fold_), no more of spine will be forced**
    one of the advantages of this is `foldr` can be used with _infinite_ list
    because `foldr` can avoid evaluating not just some or all of values in the
    list, but some or all of the list's _spine_ as well

    ```haskell
    -- this function work despite being an infinitelist
    myAny even [1..]
    -- True

    -- this function will never finish evaluating (bottom or undefined) because it's always
    -- an odd number
    myAny even (repeat 1)
    -- bottom
    ```

## day \#7: 13/01/2017

**what i have done?**

- [x] going half way of chapter 9 of `haskell book`, some examples need to be
  reviewed carefully in order to understand more about recursive implementation.

- **note:** in the coming week, i'm gonna be so busy with works, that means the
  challenge progress would be slowed down, but i do hope, that's not gonna
  affect my goals.

**lesson learned?**

- list reducer with `fold`

  ```haskell
  -- foldr
  foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr f z xs =
    case xs of
      []     -> z
      (x:xs) -> f x (foldr f z xs)

  foldr (+) 0 [1, 2, 3]
  -- 6
  ```

  + there are 2 stages when folding a list, _traversal_ and _folding_

    _traversal_ is the stage which fold recursive over the _spine_
    _folding_ refers to the evaluation of the folding function applied to  the
    values
    - one of the advantages of this is `foldr` can be used with _infinite_ list


## day \#6: 12/01/2017

**what i have done?**

- [x] watch `egghead.io`
- [x] review chapter 7 of `haskell book`
- [x] chapter 8 of `haskell book`
- [ ] almost finished chapter 9 of `haskell book`

**lesson learned?**

- recursion is self-referential composition, apply a function to an argument,
  then pass that result on as an argument to a seconds application of the same
  function and so on.
- composition function `(.) f g = \x -> f (g x)` is quite similar to recursive
  functions, the difference is that instead of a fixed number of applications,
  recursive functions rely on inputs to determine when to stop applying
  functions to successive results. without a `base` case, the result
  of `(g x)` will keep being passed back to `g` indefinitely
- understood why the author name his section as _have fun with bottom_, yes,
  i crashed my windows by passing `let x = x in x` expression in `prelude`
- a _partial function_ is one which doesn't handle all of its input
- a _full function_ is one which handles all of its input
- type synonyms/alias `type` improve the readability of type signature `type AccountName
  = Integer`
- _haskell idiom_: use generic name `go` as a helper function which could be
  defined in `where` clause for a recursive loop/function

  ```haskell
  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count ,n)
           | otherwise = go (n - d) d (count + 1)
  ```

- a _sum type_ can be read as an `or |` as in `Bool` datatype, a _product type_
  can be read asn an `and &`
- working with `list`

  ```haskell
  -- list is a recursive data structure
  data [] a = [] | a : [] -- am i watching `inception` movie here?
  ```

  + pattern matching on `list`

  ```haskell
  safeTail :: [a] -> Maybe [a]
  safeTail []     = Nothing
  safeTail (x:[]) = Nothing
  safeTail (_:xs) = Just xs
  ```

  + list syntactic sugar

  ```haskell
  -- parentheses syntax
  (1 : 2 : 3 : []) ++ 4 : []

  -- syntactic sugar syntax
  [1, 2, 3] ++ [4]
  ```

  + constructing list

  ```haskell
  -- using ranges
  [1..10]
  enumFromTo 1 10

  [1, 2..10]
  enumFromThenTo 1 2 10

  [1, 3..10]
  enumFromThenTo 1 3 10
  ```

  + extracting list element

  ```haskell
  take 7 [1..10]
  -- [1, 2, 3, 4, 5, 6, 7]

  drop 7 [1..10]
  -- [8, 9, 10]

  splitAt 5 [1..10]
  -- ([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])

  takeWhile (< 5) [1..10]
  -- [1, 2, 3, 4]

  dropWhile (< 5) [1..10]
  -- [5, 6, 7, 8, 9, 10]
  ```

  + list comprehensions

  ```haskell
  -- basic comprehension syntax
  [ x * 2 | x <- [1..5]]
  -- [2, 4, 6, 8, 10]

  -- comprehension with predicate
  [ x* 2 | x <- [1..5], x < 3 ]
  -- [2, 4]

  -- comprehension with multiple list generator
  [ x + y | x <- [1..5], y <- [1..3]]
  -- [2, 3, 4, 3, 4, 5, 4, 5, 6, 5, 6, 7, 6, 7, 8]

  -- comprehension with multiple list generator plus predicate
  [ x + y | x <- [1..5], y <- [1..3], x < 3, y > 1]
  -- [3, 4, 4, 5]

  -- comprehension with list of string
  [x | x <- "Three Letter Acronym", elem x ['A'..'Z']]
  -- TLA
  ```

- spines and nonstrict evaluation

  + data structures such as `list`, `sequence`, `tree` have _spine_ which a a
    connective structure that bind the collection of values together

    ```
    -- [1, 2, 3]
    -- or (1 : (2 : (3 : [])))
    --     :
    --    / \
    --   1   :
    --      / \
    --     2   :
    --        / \
    --       3   []
    ```

  + evaluation will proceed down the spine but when a list is built, the
    evaluation will proceed from bottom up, hence the name `lazy evaluation`?
    because `haskell` evaluation is non-strict, the list  isn't constructed
    until it's is consumed

  + use `:sprint` to see what has already evaluated

- list transformation

  + using `map`: can only be used with `[]`

  ```haskell
  -- map function
  map :: (a -> b) -> [a] -> [b]
  map _ []      = []
  map f (x: xs) = f x : map f xs


  map (+ 1) [1..3]
  -- [2, 3, 4]
  ```

  + using `fmap`: is defined in `Functor` typeclass and could be applied onto
  other datatypes as well

  ```haskell
  -- fmap
  fmap :: Functor f => (a -> b) -> f a -> f b

  fmap (+ 1) [1..3]
  -- [2, 3, 4]
  ```

  + filter values

  ```haskell
  -- filter function
  -- recursive function looks beautiful ^ ^
  filter :: (a -> Bool) -> [a] -> [a]
  filter _ [] = []
  filter pred (x:xs)
     | pred x = x : filter pred xs
     | otherwise = filter pred xs

  filter even [1..10]
  -- [2, 4, 6, 8, 10]
  ```

  + zip values

  ```haskell
  zip :: [a] -> [b] -> [(a, b)]

  zip [1, 2, 3] [1, 2, 3]
  -- [(1, 1), (2, 2), (3, 3)]
  ```

## day \#5: 11/01/2017

**what i have done?**

- [x] get my head round `function type constructor`
- [x] review chapter 6 of `haskell book`
- [x] chapter 7 of `haskell book` plus exercises

**lesson learned?**

- `function type constructor` just like other `datatypes` but also take
  `datatypes` as its argument
- unlike function application which is _left associative_, function type is _right associative_
- `newtype` keyword is a special case of `data` declaration, it only allows 1
  constructor and 1 field
- `pattern matching`: is quite similar to _function overloading_ and `if..then..else` but more powerful

  + underscore `_` act like *catch them all* expression
  + used to vary what function do given different input
  + **unpack** and **expose** the content of `datatypes`

- `case..of..where` expression
- `guards` is more like `if..then..else` expression
- [differences between `pattern matching` and `guards`](http://bit.ly/2ifgMgu)
- higher  order function is function that accept function as argument

  + function composition `(.)`: `f(x) . g(x) = f(g(x))`
  + function composition is right associative
  + function application has higher precedence than composition operator `(.)`

- _haskell idiom_: the combination of `(.)` and `($)` operators make `haskell`
  syntax look much more beautiful, easier to read and _parentheses free_, help
  you focus on the problem rather than the syntax

  ```haskell
  -- function composition
   a . b . c $ x

  -- normal function with parentheses evade
  (a (b (c x)))
  ```

## day \#4: 10/01/2017

**what i have done?**

- [x] review previous chapters of `haskell book`

  + having a hard time to reason about the difference between _left associative_
    and _right associative_

- [x] chapter 6 of `haskell book` plus exercises

**lesson learned?**

- _function application_ and `function type constructor` are different

  + while function application is _left associative_, `function type` is _right
    associative_
  + rule of thumb: _left associative_ will apply the **leftmost** first, _right
    associative_ will apply the **rightmost** first

- `typeclasses` definition with `Eq`, `Num`, `Ord`, `Show` examples
- `typeclass` inheritance is when a `typeclass` has a superclass (class
  constraint)
- `typeclasses` are deﬁned by the set of operations and values all instances
  will provide. `typeclass instances` are unique pairings of the `typeclass` and
  a `type`. They deﬁne the ways to implement the typeclass methods for that type
- a function has side effect if it modifies some state or interact with outside
  world (print?)

## day \#3: 09/01/2017

**what i have done?**

- [x] review some `prelude` commands/functions

  + quit `ghci`: `:q` or `:quit`
  + unload module: `:m`
  + load file: `:l` or `:load`
  + information: `:info` or `:i`
  + type of: `:t` or `:type`

- [x] chapter 4 of `haskell book` plus exercises
- [x] chapter 5 of `haskell book` plus exercises

**lesson learned?**

- i see some similarities between `haskell` data type and `typescript`

  + `datatype` vs. `typescript union`
  + `type constructor`, `data constructor` vs. `typescript interface`
  + `typeclass` vs. `typescript interface implement`
  + `type variables` vs. `typescript generic`
  + both have _type alias_

- predefined datatypes: `Int`, `Double`, `"String"`, `(Tuple)`, `[List]`
- conditional flow control with `if-then-else`
- `typeclass` is a set of operations defined with respect to a
  generic/polymorphic type
- `data constructor` can either have one or more arguments or be a constant
  value
- `type constructor` is **not** value and can **only be used in** type
  signatures, and used to denote `type constructor` (e.g `function type (->)`)
- `function type (->)`

  + take arguments (`type variables`) **but** has no data constructor: `data
    (->) a b`
  + is right associative
  + is _curried_ by default (or _partial application_): many functions, but
    each only take 1 argument
  + could be _uncurried_ (one function, many arguments): `(a, a) -> a` instead
    of `a -> a -> a`

- type signatures may have three different types: _concrete_, _constrained_, and
  _parametric polymorphism_
- _polymorphism_

  + _constrained or ad-hoc polymorphism_: is defined in similar to
    `typeclass`: `(+) :: Num a => a -> a -> a`
  + _parametric polymorphism_: refer to type variables that are **fully**
    polymorphic, not constrained by `typeclass` and could be **anything**: `id
    :: a -> a`

- if a variable could be anything (_parametric polymorphic_), it has no method
- if it can be some types (_typeclass instance), it has some methods
- if it is a concrete type, it lose the flexibility but gain more potential
  method

## day \#2: 08/01/2017

**what i have done?**

- [x] `.pdf` file can be viewed in `emacs 25` for my windows 10, feel much more
  productive!
- [x] chapter 2 of `haskell book`
- [x] chapter 3 of `haskell book`

**lesson learned?**

- `haskell` is non-strict (lazy) evaluation, means that not everything will get
  reduce to its irreducible form immediately
- *redex*: a reducible expression
- *infix* and *prefix* operator

  + `haskell` function has prefix syntax as default
  + arithematic operators has infix syntax: `(+)`, `(-)`, `(*)`, `(/)`
  + user \`:info\` to find information whether an operator is infix and its
    associativity and precedence
  + the higher precedence (0-9) will be applied first
  + there are *left* and *right* associative

- function application operator `$`

  + is right associative
  + very useful when you want to reduce/eliminate pairs of parentheses
  + easier to read code syntax

- `haskell` use **indentation** to define block of code expressions
- *syntactic sugar* is syntax designed to make expression easier to read or
  write (ex. `let..in`, `where`)
- when you write code in a source file, the order is unimportant, but when
  writing code directly into `REPL`, the order does matter
- `main :: IO ()` is not only function but a series of instructions to execute,
  which can include applying functions and producing **side-effects** such as
  printing to the screen.
- top-level vs local definition

  + `let` and `when` introduce local scope

- type signature `::`
- `string` type and concatenation and list functions: `++`, `concat`, `take`,
  `drop`...

## day \#1: 07/01/2017

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
    * *divergence* or *omega*: reduction process never terminates or end
      (infinite loop)

  + completed chapter 1 exercises

**lesson learned?**

- to tame `spacemacs` layout and workspace
- to have the foundation of `haskell` lambda/function.

# my goal

a new year is just around the corner and it will be very significant to my life
and my family. i'm feeling so excited yet intimidated to the challenges I set
out in new year, and this **\#100daysofcode** is one of them that i want to
complete.

however, i also want to emphasis that i'm no way a professional developer, i'm
doing this purefly because it's my passion. therefore, i'm not looking into
doing something that could give you any **wow** or **awe** moment.

here is what i hope to gain through the challenge

- step \#1: build my first ever blog, up and running

  i will use `hakyll` as a static blog generator. the reason will be explained
  in next step

  the ui will be pretty much a copy cat from default template, because i have
  zero experience with `haskell` for now

- step \#2: learning `haskell` through `haskell book`, i've been falling in love
  with this book since day one i bought it but haven't managed the time to learn
  it

  by now you would know that i will start the challenge with `haskell` because
  i'm so curious about functional programming language, about `monad` and
  everything in between. this will also help me equip knowledge to learn about
  `typescript` which is also a strong static typing language

- step \#3: re-evaluate my blog

  by the time i complete the book, i believe i will be confident enough to play
  with `hakyll` in deep, yet it's time to dust of my outdate `css3` skill.

  i will use `postcss` for my blog style

- step \#4: start learning trio `typescript+react+redux` through `egghead.io`
- step \#5 and final: build a website using everything i would have learned
  through challenge
