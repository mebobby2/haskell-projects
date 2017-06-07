# time-machine-store

# Compiling
1. Navigate to root
2. cabal configure (only needs to be ran once)
3. cabal build

# Loading Code REPL
1. ```ghci``` from root of project.
2. ```:l src/Chapter2/SimpleFunctions.hs``` load module.
3. To use function loaded: ```firstOrEmpty []```.

## If your haskell file contains imports of other modules:

4. you need to tell ghci where to find your source files. Use the command ```:set -isrc``` from inside the ghci console.
5. Or if you prefer, you can use ```cabal repl``` to start ghci. It will take into account any additional dependencies installed by cabal install your local or sandbox package repository.

# Adding new module to project
1. Choose a name for the module, for example A.B.C.
2. Create a folder for each component of its name but the last one, in this case a folder A and inside a folder B.
3. Create a file with the same name of the last component ending in .hs (here C.hs) and write the module declaration we’ve seen above. ```module Chapter2.Section2.Example where```.
4. Tell Cabal to include the file in your project, under ```exposed-modules```.

# Downloading packages from Hackage
1. Add the package to ```build-depends``` property of ```.cabal``` file
2. do 'cabal configure'. If it says it cannot find the package, then its not installed in your system, so you need to go to step 3
3. cabal install package-name


# Building executables
1. Configure .cabal file
```
executable profiling-example
  build-depends:   base >= 4
  hs-source-dirs:  src
  ghc-options:     -Wall -prof -fprof-auto -rtsopts
  main-is:         Chapter5/Main.hs
  other-modules:   Chapter5.Annotations
```
2. Chapter5/Main.hs must define an entry point to be called Main, and to contain the main :: IO () function, so the file should be named accordingly
3. cabal configure
4. cabal build
5. Run the executable: ./dist/build/profiling-example/profiling-example

# Notes

## Numbers
* Int is the bounded integer type. It supports values between at least ±536870911 (even though GHC uses a much wider range). Usually, values of the Int type have the native width of the architecture, which make them the fastest among all.
* Integer is an unbounded integer type: it can represent any value without a decimal part without underflow or overflow. This property makes is very useful for writing code without caring about bounds, but it comes at the price of speed.
* Haskell base library also bundles exact rational numbers using the Ratio type. Rational values are created using n % m.
* Float and Double are floating-point types of single and double precision, respectively.

## Negative numbers and parenthesis
Since haskell doesn’t use parenthesis in the function application, you must be a bit more careful than usual when using negative numbers. For example, if you write atan -4 in ghCi, you will get an error indicating No instance for (Num (a0 -> a0)) arising from a use of '-'. this means that it has interpreted that you are trying to compute the subtraction of atan and 4. to get the arctangent of -4 you should instead write atan (-4).

## Infix
Functions whose name is built entirely by symbols, like ++, must be called using infix syntax: writing them between the arguments, instead of in front of them. So you write a ++ b, not ++ a b. in the case where you want to use the function in the normal fashion, you must use parenthesis. So you can write (++) a b.

## Lists
As in most functional languages, lists in Haskell are linked lists. Such lists are composed of a series of cells that hold the values in a list and a reference to the next cell, and a special marker for the end of the list. The basic operations to construct lists are [] (pronounced “nil”) to create an empty list, and (:) (pronounced “cons”) to append an element to an already existing list, that is, elt:lst is the list resulting of putting the value elt in front of the list lst. So list literals can also be written as:

```
1 : 2 : 3 : []
```

Lists in Haskell are homogeneous: each list can only handle elements of one single type. Because of that, you are forbidden to create a list containing integers and characters, and also to concatenate two lists with different kind of elements

If you try to get the head or the tail of an empty list, we get an error, as you may expect. Be aware that exceptions are not the preferred way to handle errors in Haskell (you will see why in more detail in subsequent chapters) and by default make the entire program crash when found. To prevent errors from operations on empty lists, just be sure to check for non-emptiness before applying functions such as head and tail (or use pattern matching, another syntax that will be introduced shortly).

Lists can contain other lists as elements (or to any level of nesting). As [t] are lists of type t, lists or lists would be [[t]]. The inner lists inside the outer lists need not to be of the same length (so they are not equivalent to arrays of multiple dimensions). One important thing to remember is that an empty list can be a member of a larger list of lists, so [] and [[]] are not equivalent: the first is a completely empty list of lists, whereas the second is a list which contains only one element, which is an empty list.

## Number precision
The usual warnings about comparison of floating-point values apply here. Computers are not able to represent with exact precision all the values, so you may find that equalities that you expect not to hold actually do. For example, in my system the expression (4.00000000000000003 - 4) == 0 evaluates to True.

## Local bindings
Gives name to an expression to be used in a larger one. There are two kinds of binding constructs in Haskell: let and where. In both cases a binding is introduced by name = expression. The difference lies in the position over the main expression: let introduces bindings before the main expression, and must end with the in keyword. On the other hand where does so after the expression.

## Indentation
A first guess about the reason may lead you to think about indentation-sensitive languages like Python. However, Haskell uses a different solution, called layout. In a layout-based syntax how a line is indented isn’t as important as the fact that all elements in the same block start in the same column. For example:
* In an if block, the lines for then and else must be indented the same way.
* In a let or a where block, all local bindings must start in the same position.

## Tuples
Haskell provides tuples to group a fixed number of components of different types and lists to contain an unlimited number of elements of a homogeneous type. Homogeneous means elements inside tuples can be of different type.

## ADT
The most basic kind of data types that you can create in Haskell are called algebraic data types (or ADTs for short) and will be the focus of this section. An ADT is defined by two pieces of data:
* A name for the type that will be used to represent its values,
* A set of constructors that are used to create new values. These constructors may have
arguments that will hold values of the specified types.

Some insights of ADTs:
* If you tried to create a completely new ADT, for example named Client2, but you used
the same constructor names, you will get a build error. This is because inside a module all constructors must have different names. If you think about it, it’s sensible to ask for that condition, because otherwise the compiler wouldn’t be able to distinguish which type you are willing to create,
* Data types and constructor names live in different worlds. That means that it is possible to create a constructor with the same names as a data type. Indeed, it’s a common convention for one-alternative types, such as Person, to have the two names coincide,
* For being able to use the default deriving functionality, all types used inside another one must be showable. For example, if you didn’t include deriving Show in Person, a compilation error will be signaled.

## Capitalization in Haskell
One of the special characteristics of haskell syntax is that names given by the user must follow some capitalization rules. here is a brief summary of the conventions:
* Functions, parameters and bindings must start with a lowercase letter. in case of an operator name, it must not start with :.
* types, constructors, type classes and kinds must start with an uppercase letter. if using an operator name, it must start with :.

## Pattern matching
I strongly emphasize the fact that pattern matching does not backtrack when something goes wrong in the body of a match. this is important to remember, especially if you are coming from a logic programming background in which unification with backtracking is the norm.

Example:
```
f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _                                  -> "There is no boss"
```

and

```
g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos ->
               case pos of "Boss" -> name ++ " is the boss"
             _                               -> "There is no boss"
```

are not equilvalent.

```
 f (Company "A" 5 (Person "John" "Jefferson" Male) "Director")
 "There is no boss"

 g (Company "A" 5 (Person "John" "Jefferson" Male) "Director")
 "*** Exception: Non-exhaustive patterns in case
```

When the value is given to f, the first pattern does not match, because "Director" is not equal to "Boss". So the system goes into the second black-hole match and sees that there is no boss. However, on g it first matches into being a Company, which the value satisfies, and in this point it enters the body of the match and forgets about other alternatives. Then, the inner match fails, raising the exception.


## Order of evaluations?
How does Haskell evaluate this expression?
```reverse2 (tail list) +++ [head list]```

1. reverse2 (tail list) +++ [head list]

```(tail list)``` is in brackets, so that is evaluated first. Lets put the result of ```(tail list)``` into a temporary binding called tail_results

2. reverse2 tail_results +++ [head list]

In Haskell, function application has precedence over operators. ```reverse2 tail_results``` is a function application whereas ```+++``` is an operator application. Hence we evaluate ```reverse2 tail_results``` first. Lets put the results into a temporary binding called reverse_tail_results

3. reverse_tail_results +++ [head list]

```[head list]``` is also a function application, so it gets evaluated before the infix operator ```+++```. Lets put the results into a temporary binding called [head_list]

4. reverse_tail_results +++ [head_list]

```+++``` is an infix operator, meaning it can be written as ```+++ reverse_tail_results [head_list]```. So the final evaluation is a function application of ```+++``` with two arguments reverse_tail_results and [head_list].

## as patterns
```
sorted (x:y:zs) = x < y && sorted (y:zs)
```

There is still some repetition in this code: I am matching on y:zs just to later reconstruct it. This sequence of checking whether some value conforms to some pattern, but later to use the value as a whole and not its components, is quite common in Haskell code. For that reason, Haskell introduces a syntactic form referred to by the term as patterns. The as patterns allows you to bind some value in the match, while at the same time allowing you to match on inner components of that value.

```
sorted (x : r@(y:_)) = x < y && sorted r
```

## Records
The concept of a data structure that fields which can be accessed by name does exist in Haskell. Records make accessing or updating part of a structure much easier than otherwise. Records are defined using data declarations, but instead of just using a type for each parameter, we write parameter name :: parameter type. These declarations are the only exception to the layout rule: we always need to write the set of fields between { and } and to separate them by commas.

Field names are also used to create special functions that access those particular fields.

## ($)

```
($) :: (a -> b) -> a -> b
f $ a = f a

```

Why is this ($) function useful at all? At first glance, it seems like a rather cumbersome way to apply a function to some arguments, given that that is the main use of functions. But apart from this definition, Haskell gives a very low precedence to ($), so both sides of this operator will be evaluated before f is applied to a. Therefore, we can omit a lot of parenthesis when using ($). This is very common in Haskell. For example, the following:

```
maximum (map succ [1, 2, 3])
```

Can be written as :

```
maximum $ map succ [1, 2, 3]
```

## Anonymous Functions

not all forms of regular function declarations are allowed when used anonymously. Anonymous functions don’t have a name, so they cannot call themselves, thus forbidding recursion. Furthermore, only one pattern can be matched. So if you want to match several patterns, you must resort to a case statement.

When a function encloses the values from the surrounding environment along with the body, these functions are usually known as closures in almost all languages supporting functional features.

## Understanding point free style

```
double list = map (\x -> x * 2) list
```

Can have the ```list``` argument omitted because ```list``` is in the end of both parameter lists. So, it can be written as:

```
double = map (\x -> x * 2)
```

Currying makes omitted the arguments possible. This is how:

1. map function has type:

```
map :: (a -> b) -> [a] -> [b]
```

2. The annoymous func ```(\x -> x * 2)``` has type:

```
(\x -> x * 2) :: Integer -> Integer
```

3. We now partially apply the annoymous func to map, since the annoymous func is map's first argument. The partially applied map func now looks like:

```
map (\x -> x * 2)
```

Which because of automatic currying, returns another function which takes lesser arguments. The type of this curried function is:


```
map (\x -> x * 2) :: [Integer] -> [Integer]
```

4. Now you can call this current function like this:

```
curried = map (\x -> x * 2)
curried [2,3,4]
```

And knowing this, you can just change the double to return this curried function, like this:

```
double = map (\x -> x * 2)
```

5. However, following this path of partial application, we can apply it to the annoymous block ```\x -> x * 2```. We partially the number 2 to the function ```*```. And we get the curried function:

```
(*2)
```

Which can be called like this:

```
curried = (*2)
curried 4
```

6. From the previous step, we know that the curried function ```(*2)``` is equilvalent to the annoymous block ```\x -> x * 2```.
And so, the double function's definition can be further reduced to:

```
double = map (*2)
```

Once you know about the possibility of partially applying functions, it’s time to look more deeply into the meaning of the function types as they are written. First of all, the -> symbol binds to the right. That is, the type a -> b -> c -> d is a prettier, but equivalent, version of a -> (b -> (c -> d)). So, at its core, every function with more than one parameter is just a function that takes one parameter and returns a closure with one parameter less, which may indeed consume another parameter and so on, until we reach a non-function type.

Partial application encourages a programming style where functions are combined without ever mentioning their parameters. This is called point-free style (because in mathematics, parameters to functions are called points).

## Function composition
Is when the period applies one function after the other. For example, following is how to write function f applied to the output from g:

```
f . g = \x -> f (g x)
```

## Currying
Functions that take a sequence of arguments are called the curried versions of those that take a tuple. I’ll stress the subtle difference: the not curried version of a function only takes one argument, but it is a tuple, so in one value it holds more than one piece of information.

Usually we prefer these curried versions, because we can partially applicate them. But sometimes an uncurried version is also interesting to consider. For example, say you are given a list of pairs of numbers, and you want to get the list of the maximums of pairs. You cannot directly use map max, because max requires two arguments. The solution then is to curry the function before application:

```
map (uncurry max) [(1,2),(2,1),(3,4)]
```

```
:t max
max :: Ord a => a -> a -> a

:t uncurry max
uncurry max :: Ord c => (c, c) -> c
```

## folds
The name foldr is a reminder of the algorithm the function implements. It is a fold which associates to the right. That is, the innermost parenthesis will be found in the right side of the expression. Similarly you can build a fold that associates to the left, which is included in the Haskell Platform as foldl.

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (a -> b -> a) -> a -> [b] -> a
```

## Operator and sections
Lets say we want to write this anonymous block using partial application:

```
blk = \x -> x * 2
```

When the function has an operator name (only with symbols, like *), we cannot just use its name and arguments, but we need to use a section. A section is just a specification of the operation to be done, enclosed in parenthesis, and where the parameters for the anonymous function are completely wiped out.
In this case, the new definition is:

```
blk = (*2)
```

If the block took 2 parameters, it can also be partially applied using a section:

```
blk = \x y -> x * y
blk = (*)
```

## nub and nubBy
These two functions take out elements such that no two elements in the returned list are equivalent.

nub and nubBy are not very performant functions, as they must check for equality between all possible pair of elements. This means that the order of the function is quadratic (n^2 time complexity).

## multiline blocks in CHCI
Done using ```:{``` and ```:}```. Eg:
```
Prelude> :{
Prelude| 1/2 +
Prelude| 1/3
Prelude| :}
```

## backtick
Use backtick to turn a function into an operator. Eg:
```
intersect [1,2,3] [2,3]
[1,2,3] `intersect` [2,3]
```

## Haskell is declarative
You may wonder why Haskell provides so many different functions on lists, whereas other programming languages just do fine with constructs such as iterators or for loops. The idea is that instead of explicitly transforming a list element by element, you declare transformations at a higher level of abstraction. Languages supporting this idea, such as Haskell, are called declarative.
A classical fear when programming in Haskell is that this higher level of abstraction hurts performance. However, compilers of declarative languages are able to apply a wider range of optimizations, because they can change the code in many more ways while retaining the same behavior. A typical example code in the form map f . map g. This code performs multiple passes over the data, but can safely be converted by the compiler to map (f . g), which performs the same duty in just one pass over the data.

## List comprehensions
If you remember your algebra classes, mathematicians have a very terse but intuitive language for manipulating sets. The previous example can be written in set notation as  2x | x list.odd (x)  . Haskell designers also like this syntax, so they included list comprehensions to mimic it.

List comprehensions have two parts, separated by | and wrapped by square brackets. The first part is the expression, which defines a transformation to apply to all the elements that will be returned. The second part is made of a list of qualifiers, and specifies from whence the elements will come, and the constraints upon them.

The first kind of qualifiers are generators, which take the form e <- list. Generators indicate that elements from list will be acted upon, and each of the elements will be referred as e in the rest of the comprehension.

```
doubleOdds list = [ 2 * x | x <- list, odd x ]
```

## Data.Map
Maps are always shown with its keys ordered. The map itself maintains that invariant, so it can easily access the maximal and minimal elements in the map. Indeed, functions such as findMin/findMax, deleteMin/deleteMax, and updateMin/updateMax take advantage of this fact, and allows fast retrieving, deletion, or updating of the values associated to those keys.

## IntMap, IntSet, HashMap, and HashSet
Maps can be made much more efficient if you use only integers as keys. The same happens for sets holding only integer values. For that reason, the containers library provides specific data types for that purpose, namely IntMap and IntSet.

Alternatively, the keys on a map or values on a set may not be integers but could be mapped almost uniquely to one. This mapping is called a hash of the original value. The types HashMap and HashSet in the unordered-containers package provides implementations of maps and sets whose keys and elements, respectively, can be hashed, and which is much more efficient than its regular counterparts, if the type admits to be hashed.

## Type synonyms
The type keyword, which we haven’t yet introduced, is used to create type synonyms, that is, giving an alternative name to a type. Usually, it’s used to call a large type by a smaller or more expressive name. For example, you may introduce the following synonym for those functions returning a Boolean value:

```
type Predicate a = a -> Bool
```

The type synonym and its expansion are interchangeable in all circumstances. That is, you can also write the type of filter as Predicate a -> [a] -> [a], and the compiler would be fine with it. In contrast, the other way to define alternative names, using newtype, doesn’t make the types equivalent.

## Trees
There are several ways to visit a tree (that is, traversing all of their elements), which are broadly divided in two families: depth-first traversal and bread-first traversal. In the former case, each node of the tree recursively visits its subtrees. There’s still a choice of when to visit the value in the node itself: before any subtree (pre-order) or after all subtrees are visited (post-order).

## Type classes
```
M.insert :: Ord k => k -> a -> M.Map k a -> M.Map k a

```
Notice how Ord k is separated from the rest of the type by => (not to be confused by the arrow -> used in the type of functions). The purpose of Ord k is to constrain the set of possible types that the k type variable can take. This is different from the parametric polymorphism of the list functions in the previous chapters: here you ask the type to
be accompanied by some functions. This kind of polymorphism is known as ad-hoc polymorphism. In this case, the Ord type class is telling that the type must provide implementations of comparison operators such as < or ==. Thus, it formalizes the notion of "default order".

Eq is the type class declaring that a type supports checking equality (and inequality) between their values. Let’s look at its definition from the GHC source code

```
class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    x == y = not (x /= y)
```

At the beginning of the section it was stated that type classes only include the declaration of functions to be implemented, but here you find some code implementation. The reason is that those are default definitions: code
for a function that works whenever some of the rest are implemented. For example, if we implement (==), there’s a straightforward way to implement (/=), as shown above. When instantiating a type class, you are allowed to leave out those functions with a default implementation.

This means that when implementing Eq, you may do it without any actual implementation, because all functions have default implementations. In that case any comparison will loop forever, as (/=) calls (==), which then calls (/=), and so on indefinitely. This may lead to the program crashing out of memory, or just staying unresponsive until you force its exit. For preventing such cases, type classes in Haskell usually specify a minimal complete definition: which set of functions should be implemented for the rest to work without problems. For Eq, the minimal complete definition is either (==) or (/=), so you need to implement at least one.

## Importing instance declarations
When you use a type that implements a class, the Haskell compiler must look for the corresponding instance declaration. It does so by looking inside all the modules that are imported, independently of the way of importing them. Currently, it’s not possible to prevent an instance declaration being imported. This means that if you find in some source code

```
import Module ()
```
it may not be an error (what's the point of having such a declaration if nothing is imported?), but rather an import of the instance declarations found in Module.

## Containers
Haskell has several data containers, including maps, sets, trees, and graphs. The quest for understanding its commonalities has led to the discovery of the concepts of functor and foldable.

## Monoids
In Haskell, a monoid is a type with a rule for how two elements of that type can be combined to make another element of the same type. To be a monoid there also needs to be an element that you can think of as representing 'nothing' in the sense that when it's combined with other elements it leaves the other element unchanged.

A great example is lists. Given two lists, say [1,2] and [3,4], you can join them together using ++ to get [1,2,3,4]. There's also the empty list []. Using ++ to combine [] with any list gives you back the same list, for example []++[1,2,3,4]==[1,2,3,4].

Some Uses of Monoids:

One reason is that with a monoid we get another function called mconcat for free. mconcat takes a list of values in a monoid and combines them all together. For example mconcat [a,b,c] is equal to a `mappend` (b `mappend` c). Any time you have a monoid you have this quick and easy way to combine a whole list together. But note that there is some ambiguity in the idea behind mconcat. To compute mconcat [a,b,...,c,d] which order should we work in? Should we work from left to right and compute a `mappend` b first? Or should we start with c `mappend` d. That's one place where the associativity law comes in: it makes no difference.

Explicitly using the Monoid type class for a function also tells the reader of your code what your intentions are. If a function has signature [a] -> b you know it takes a list and constructs an object of type b from it. But it has considerable freedom in what it can do with your list. But if you see a function of type (Monoid a) => a -> b, even if it is only used with lists, we know what kind of things the function will do with the list. For example, we know that the function might add things to your list, but it's never going to pull any elements out of your list.

## Functors
A data type supporting a function like map is called a functor. The corresponding class is defined as:
```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
You may notice a strange fact about the Functor class: in the definition of fmap the type variable corresponding to the instance is applied to another type variable, instead of being used raw. This means that those types that are to be functors should take one type parameter. For example, ```IntSet```, which takes none, cannot have such an instance (even though conceptually it is a functor), whereas ```Tree a``` takes one type parameter ```a```.

The way in which the Haskell compiler checks for the correct application of type parameters is by the kind
system. Knowing it may help you making sense of some error messages. Until now we know that values, functions and constructors have an associated type; but types themselves are also categorized based on the level of application. To start with, all basic types such as Char or Integer have kind *. Types which need one parameter to be fully applied, such as Maybe, have kind * -> *.This syntax resembles the one used for functions on purpose: if we now have Maybe Integer, we have a type of kind * -> *,which is applied a type of kind *. So the final kind for ```Maybe Integer``` is indeed *.

If we want to make a type constructor an instance of ```Functor```, it has to have a kind of ```* -> *```, which means that it has to take exactly one concrete type as a type parameter. For example, ```Maybe``` can be made an instance because it takes one type parameter to produce a concrete type, like ```Maybe Int``` or ```Maybe String```. If a type constructor takes two parameters, like ```Either```, we have to partially apply the type constructor until it only takes one type parameter. So we can't write ```instance Functor Either where```, but we can write ```instance Functor (Either a) where```.


Examples of Functors:
```
map   :: (a -> b) -> ([a]       -> [b])
M.map :: (a -> b) -> (M.Map k a -> M.Map k b)
fmap  :: (a -> b) -> (T.Tree a  -> T.Tree b)
```
We should remark that Set cannot be made an instance of Functor. The reason is that the mapping function for sets require all elements to be part of type class ```Ord```, i.e. has the type
```
Ord b => (a -> b) -> Set a -> Set b
```
which is not compatible with that of Functor, which doesn't have any restriction.

## Why Prelude includes specialized definitions for lists
You may wonder why the Prelude includes specialized definitions for lists instead of the most general versions using Functor and Foldable. The reason is that for a beginner, having the functions working only on [a] helps understanding the first error messages that it may encounter, because they don't involve type classes. But now that you know about them, you should aim for the largest degree of abstraction that you can achieve. Type classes allow generalizing the values it can take, therefore increasing re-use and abstraction.

## Lazy evaluation model
Most of the programming languages follow a strict evaluation model: at the very moment a compound expression is found, it’s immediately transformed into a simpler version (maybe including calls to functions of methods), before the larger expression is evaluated. In particular, arguments to a function are evaluated before the control flow enters the body of the function itself. Haskell uses non-strict evaluation, also known as lazy evaluation.

By default, Haskell only evaluates an expression until a constructor is found. The rest of the value will be left unevaluated: its place will be occupied by a placeholder indicating how that specific field can be computed. This placeholder is called a thunk. E.g:
```
data TimeMachine = TM { manufactuer :: String, year :: Integer } deriving (Eq, Show)
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)
timelyIncMachines = timeMachinesFrom "Timely Inc." 100


head timelyIncMachines
```
timeMachinesFrom will just produce a (:) constructor with a thunk for the element and another thunk for the rest of the list. When you apply head to it, you get back the first of the thunks. If you want to show the value on the screen, the thunk has to be unwrapped and the recipe to create the value followed to create it.

One important feature of lazy evaluation is that once a thunk has been evaluated, the result is saved and the thunk is not evaluated again if the result of that expression is needed elsewhere. This is a great feature, because it means that you only pay once for the cost of evaluating each expression in your application. Furthermore, the pure nature of Haskell also helps in sharing thunks that refer to the same expressions, which means that it can re-use the evaluation in some part of the program in other places.

It should be noted that only expressions will be shared. This should not be confused with memorizing a function: that is, caching the results for arguments that have already been provided. For example, if you call:
```
(allNumbersFrom 1, allNumbersFrom 2)
```
Even though allNumbersFrom 1 will call allNumbersFrom 2, the evaluation of allNumbersFrom 2 in allNumbersFrom 1 and in the previous expression will not be shared.

This would be impossible in a language that allows printing while computing a value. Let’s assume than during its evaluation, allNumbersFrom outputs "Natural numbers rule!". If you share the same value for allNumbersFrom, the string would only be printed once. But in many languages, including C and Java, what you would expect is to show it three times, one per reference to allNumbers. You have seen that side effects make it impossible to apply these sharing optimizations, which are key to a good performance in Haskell programs.

One final issue that remains to be explained is how cyclic structures are represented. The answer is that Haskell maintains itself a cycle in memory when declarations are the same. E.g. ```repeat e```

thunk 1     =  e

thunk 2     = repeat e

constructor = :

=> e : repeat e

This is cyclic so, so we can remove thunk 2 and replace it with a pointer back to the constructor.

## Evaluation Strategies
We have seen two examples of evaluation strategies: ways in which the computation proceeds and the order in which parts of the expressions are evaluated. Apart from those two, more strategies have been developed.

What I have called strict evaluation is also known as call by value. Sometimes, especially in object-oriented languages, this is changed to call by reference, where you don’t receive values as arguments, but boxes holding those values.

Lazy evaluation is sometimes referred to as call by need, which is a special case of the more general strategy of call by name, in which function arguments are not evaluated before the body of the function but are substituted directly. The difference is that in general, call by name may evaluate the same expression more than once, whereas call by need uses thunks to do it only once.

## Problem with laziness
Laziness is often a blessing, but sometimes it can also be a curse. As usual in computer science, there’s a trade-off in lazy evaluation: in this case, delaying the evaluation until needed may result in less computation and also allow some programming idioms unavailable in other languages. On the other hand, it may create many thunks, causing the memory to become quite full so that the operating system has to paginate, which makes the program slower.

E.g. ```foldr (+) 0 [1 .. 1000000000]``` evaluates to ```(1 + (2 + (3 + (... + <thunk>))))```. This reults in an out of memory exception because too many thunks are created and store in memory. It evaluates it this way because at each point during evaluation it only knows about one argument to (+).

So, if we use parentheses in another way, making the evaluation to look like ```((((1 + 2) + 3) + ...) + <thunk>)```, the problem may be gone. You already know how to do it: using foldl: ```foldl (+) 0 [1 .. 1000000000]```. But here you face here a similar situation: (+) has at each step all of its arguments, but since you do not request the result until the end of the list, a large amount of thunks have to be created.

The solution is to force evaluation: you need to tell Haskell to evaluate the (n+m) thunks before proceeding with the rest of the computation, overriding the default lazy behavior.

We stress that Haskell only evaluates something until a constructor is found: the fields are left as thunks until some further computation needs the information enclosed by them. This is true also for seq: if you want to be sure that some part of a larger value is evaluated before continuing, you should explicitly get that value and force it.

Now that you know about forcing evaluation, you should resist the temptation to use it everywhere you think a memory problem could be found. Forcing expressions destroys the lazy nature of the language and may also lead to cases where a previously terminating expression no longer is. Think of the case of taking the head of an infinite list: if you make Haskell force the entire list, it will never reach the end, thus entering in an infinite computation chain. If you suspect a memory leak, you should first use profiling to find the correct spot and then think carefully whether using seq will not hurt the applicability of your functions to the kind of arguments that are expected.

## Irrefutable pattern
Interesting enough, and because of this evaluation forcing in pattern matching, Haskell also includes a way to delay evaluation in matching phases. The way to do it is to use an irrefutable pattern: matching upon it never fails, but it’s only destructured when some of its constituent parts are needed. One use case for irrefutable patterns involves a function that always returns a value given the same input. For example, you are finding an element in a list, and you have made sure that the element you are searching for already exists, so find will always return the same result. In that case, you can delay the computation of find a bit, and just evaluate it when you need the constituent value.

For a more explicit example, suppose that you have some function:

```
lengthyOperation = if lengthyPredicate then Just something else Nothing
```
and that you know that the lengthyPredicate will be true in some situation. If you write a regular matching:

```
case lengthyOperation of
  Just something -> ...
  Nothing        ->
```

then you will force the lengthyOperation to be evaluated just to choose the branch. But since you know that the first one will be the selected one, you can delay the computation a bit more using an irrefutable pattern:

```
case lengthyOperation of
  ~(Just something) -> ...
```

Remember that a pattern such as that never fails. So if you come to a situation where lengthyOperation returns Nothing, and you use something inside the body of the match, you will get an error.

Irrefutable patterns are rarely used, much less than forcing, but in some cases they are key to a good performance of the code. You shouldn’t worry too much about understanding all the cases where they may be applicable, but knowledge about their existence may become handy, especially if reading the inner of some built-in function.

## undefined
It won’t be long until you read in the documentation of some package that a function is strict on one or several of its arguments. At a high level, it means that the argument will have to be evaluated if it was still in thunk form, so you should take care of providing in that place an expression that wouldn’t lead to non-termination.

Formally, in Haskell we have a canonical value called undefined that represents all those computations that don’t end. Because it never returns, it may be typed as you want, so we have that ```undefined :: a```. By the way, this typing makes undefined a perfect placeholder in the place of code you haven’t yet written, when you want to check that your current code passes type checking.

E.g: ```let (x,y) = (undefined,"hello!") in y```

A function f is then called strict on its argument if f undefined = undefined, that is, if given a non-terminating argument, the function itself does not terminate. One example of strict function is head. But a function defined as g x = 1 isn’t, because, given any argument, it returns 1.

Intuitively, the notion of being strict on something means that it doesn’t inspect that something. The way a function is strict may be subtler than in the previous examples. For example, head undefined is undefined, but head (1 : undefined) isn’t.

## Strictness annotations
In general, you can think of a value in Haskell being represented in memory as some header stating its type and the constructor used to build it, followed by references to each of the fields composing that value. Basic types, such
as integers or characters, deviate from this layout and are represented just by the header and the value itself.

Remember that before being completely evaluated, expressions in Haskell are represented by thunks. This representation is very flexible but suffers from some performance penalties. First of all, you may be creating thunks for values that you know will be used in the future or that may be needed to ensure good performance for that data type. For example, if you implement a new kind of list that stores its length, it doesn’t make much sense to not store the length directly and instead evaluate it lazily, because at the moment you need to query it, a long chain of computations will happen and performance will suffer.

In that case you want the length to be a strict field. Following the same syntax of bang patterns in matching, strict fields are declared by writing ! before the type of the field itself. As a result, every time a new value of that type is created, the expressions in the strict positions will be forced to evaluate in the same fashion as if you had included an explicit seq. A possible implementation of our lists with length could be:

```data ListL a = ListL !Integer [a]```

The memory representation of values also makes generous use of references to associate values to field positions. This means that every time you want to access a field in a value, we need to traverse one reference. Once again, this is very flexible and allows to have potentially very extensive structures in memory but can be overkill for accessing small fields, such as integer ones, whose value could be directly encoded in the space that is taken by the reference (the size of a pointer in the target architecture). A field in that situation is said to be unpacked.

Unpacking fields is a special feature of the GHC compiler and it’s declared via an {-# UNPACK #-} annotation right before the field declaration. For example, you could decide to unpack the identifiers of all the constructors of the Client data type to make it more efficient:

```
data Client = GovOrg {-# UNPACK #-} !Int String
            deriving Show
```

It should be noted that not all fields can be unpacked: it depends on the type of field. Basic types, such as integers or characters, are eligible. Other data types can only be used if they consist of just one constructor and all their fields are also unpacked: this makes it possible to unpack a tuple of types that are unpackable themselves but forbids unpacking a list. Trying to unpack a String field will also produce a warning, since it’s really just a list of Char.

In many cases, you should consider if for your particular application you prefer a lazier or a stricter implementation of our data structures. Laziness delays the moment of evaluation and allows computing only what is strictly needed for the program, but has the trade-offs of larger memory consumption and more uncertainty over when the evaluation will take place.

## k-means
The number of partitions to create is usually represented as k, and must be explicitly given as input to the algorithm. This need to specify the number of partitions up front is one of the shortcomings of K-means. Different methods are proposed in the literature to determine the best value to provide.

K-means works only on vectors, for which you can define a notion of distance and proximity.

## FlexibleInstances
The Haskell Report only allows instance declarations for types whose shape is a name followed by a list of distinct type variables. The above definition doesn’t follow that lead, so the compiler complains. However, GHC supports those declarations if you enable the FlexibleInstances extension. So you can write stuff like this:
```
instance Vector (Double, Double) where
  distance = 1.0
```

## Template Haskell
Template Haskell is the name of a metaprogramming facility included in GHC. Metaprogramming is the name given to those techniques that allow modifying the code that will be generated by a compiler, usually generating new code automatically. In the language Lisp, metaprogramming is a form of compile-time macros.

ou’ve already seen an example of metaprogramming: the deriving mechanism for built-in type classes. Template Haskell provides an extensible interface to the GHC compiler and allows library authors to provide their own code modification facilities, as the lens library does. There are many other libraries in Hackage making use of Template Haskell: for example, derive includes automatic derivation of many other type classes, such as NFData.

Template Haskell is not part of the Haskell 2010 Report so, as usual, your code won’t be easily portable to other Haskell compilers as is stands. However, GHC provides a command-line argument, -ddump-splices, which outputs the code that Template Haskell generated, and you can copy it back if you need full compatibility.

## Hanging Lambdas
The style of writing the argument to a function in a different line from the body is called “hanging lambdas.” It’s very common when using function combinators, such as our thenDo.
```
thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo` (\n ->
    productIdByPurchaseId purchaseId `thenDo` (\productId ->
    priceByProductId productId       `thenDo` (\price ->
    Just $ fromInteger n * price      )))
```

## Combinator
There is no formal definition of a combinator, but usually in the haskell community, a combinator is a function that glues other functions together to perform a certain task. ```approxSqrt x = round (sqrt x)``` can be re-written using the combinator (.), ```approxSqrt = round . sqrt```. ```thenDo``` in the above point is a user defined combinator.


## Monad
This type class encompasses all those types that allow combining computations of a certain kind between them. You have already seen two examples:

* The Maybe monad combines functions that may fail
* The State s monad combines functions which keep track of an internal state of type s

The Monad class supports these functionalities.
1. ```>>=``` binding
2. ```return``` wrap a pure value using a monad
3. ```>>``` combines two computations such that the second one doesn't use the return value of the first
4. ```fail``` allows special behaviour of the monad when some part of its computation fails.

In summary, a monad is an abstraction that allows us to write imperative (or sequential) that manipulates state, and can either fail or return a result.

How monads came about?
From the similarities in the code using Maybe and using State the notion of monad was devised: a way to combine computations with some special characteristic, such as being able to fail or having an internal state.

You have seen several other monads: Reader, which holds a read-only context; Writer, which outputs a write-only value that is combined using a monoid structure; RWS, which combines the three Reader, Writer, and State monads; and ST, which implements controlled mutable variables.

## do notation
Haskell gives special syntax for monads: the so-called do notation.
E.g.
```f >> g``` is written as:
```
do f
   g
```

```f >>= (\x -> g x)``` is written as
```
do x <- f
   g x
```

There’s also support for introducing computations that are not done inside a monadic context. For example, you may need to call (+) over a number that has been obtained before. But if you do the following:

```
do number1 <- obtainNumber1  -- you can specify any Maybe value, such as Just 3
   number2 <- obtainNumber2  -- you can specify any Maybe value, such as Just 5
   sum     <- number1 + number2
   return $ sqrt sum
```

The compiler will complain because the addition doesn’t have the required return type, which should be m a, where m is a monad. One solution is changing the previous-to-last line to ```sum <- return $ number1 + number2```, but this introduced an unnecessary burden. The best thing is to use a let expression:

```
do number1 <- obtainNumber1  -- you can specify any Maybe value, such as Just 3
   number2 <- obtainNumber2  -- you can specify any Maybe value, such as Just 5
   let sum = number1 + number2
   return $ sqrt sum
```

Notice that you don’t have to write in after this kind of let expression.

You can use pattern matching directly on let and where blocks and function declarations. This possibility is also available when using <- or let in a do block. If you remember, this had the risk of the returning value not matching the pattern. In those cases, the compiler added automatically a call to error with the appropriate message. When using do notation, the behavior deviates a bit from this: instead of calling error, the compiler will call the fail function of the monad. For example, the code:

```
do True <- willThatHold -- placeholder for a function returning a Maybe Bool value
   f 5
```

Would be generate to a version with a explicit branch for those values that are not True, even if that part didn’t appear in the code:

```
willThatHold >>= \x ->
  case x of
    True -> f 5
    _ -> fail "error"
```

The great power of do blocks comes from the fact that they are not limited to just two expressions: the syntax is desugared also for more expressions. For example, if you have:

```
do x <- f
   g
   y <- h x
   return y
```

This is more readable than its corresponding translation:

```
f >>= (\x -> g >> (h x -> (\y -> return y)))
```

## Lambda with no arguments
In haskell, there are no such thing as lambdas with no arguments. E.g.
```
reverse          :: [a] -> [a]
reverse          =  foldl (flip (:)) []
```
```(flip (:))``` looks like a lambda without any arguments, but its just a normal partial function application. The compiler will compile if you try doing this: ```(-> 2) + 2```.




# Book source code

https://github.com/apress/beg-haskell

# Maths Definitions

* Homogeneous - of the same kind; alike. E.g. An array that holds only one type

* Associative - involving the condition that a group of quantities connected by operators gives the same result whatever their grouping, i.e. in whichever order the operations are performed, as long as the order of the quantities remains the same, e.g. ( a × b ) × c = a × ( b × c )

* Commutative - involving the condition that a group of quantities connected by operators gives the same result whatever the order of the quantities involved, e.g. a × b = b × a.

* Heterogeneous - incommensurable through being of different kinds, degrees, or dimensions. E.g. An array containing two or more different types

# Upto

Page 157
