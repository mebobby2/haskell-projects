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
Add the package to ```build-depends``` property of ```.cabal``` file

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

## Monoids
In Haskell, a monoid is a type with a rule for how two elements of that type can be combined to make another element of the same type. To be a monoid there also needs to be an element that you can think of as representing 'nothing' in the sense that when it's combined with other elements it leaves the other element unchanged.

A great example is lists. Given two lists, say [1,2] and [3,4], you can join them together using ++ to get [1,2,3,4]. There's also the empty list []. Using ++ to combine [] with any list gives you back the same list, for example []++[1,2,3,4]==[1,2,3,4].

Some Uses of Monoids:

One reason is that with a monoid we get another function called mconcat for free. mconcat takes a list of values in a monoid and combines them all together. For example mconcat [a,b,c] is equal to a `mappend` (b `mappend` c). Any time you have a monoid you have this quick and easy way to combine a whole list together. But note that there is some ambiguity in the idea behind mconcat. To compute mconcat [a,b,...,c,d] which order should we work in? Should we work from left to right and compute a `mappend` b first? Or should we start with c `mappend` d. That's one place where the associativity law comes in: it makes no difference.

Explicitly using the Monoid type class for a function also tells the reader of your code what your intentions are. If a function has signature [a] -> b you know it takes a list and constructs an object of type b from it. But it has considerable freedom in what it can do with your list. But if you see a function of type (Monoid a) => a -> b, even if it is only used with lists, we know what kind of things the function will do with the list. For example, we know that the function might add things to your list, but it's never going to pull any elements out of your list.

# Book source code

https://github.com/apress/beg-haskell

# Upto

Page 108
Container-related Type Classes
