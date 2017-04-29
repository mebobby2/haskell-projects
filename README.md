# time-machine-store

# Compiling
1. Navigate to root
2. cabal configure (only needs to be ran once)
3. cabal build

# Loading Code REPL
1. ```ghci``` from root of project.
2. ```:l src/Chapter2/SimpleFunctions.hs``` load module.
3. ```firstOrEmpty []``` call function.

# Adding new module to project
1. Choose a name for the module, for example A.B.C.
2. Create a folder for each component of its name but the last one, in this case a folder A and inside a folder B.
3. Create a file with the same name of the last component ending in .hs (here C.hs) and write the module declaration we’ve seen above. ```module Chapter2.Section2.Example where```.
4. Tell Cabal to include the file in your project, under ```exposed-modules```.

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



# Book source code

https://github.com/apress/beg-haskell

# Upto

Page 54
Once you know about the possibility of partially applying functions, it’s time to look more deeply into the meaning of the function types as they are written