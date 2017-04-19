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

# Book source code

https://github.com/apress/beg-haskell

# Upto

Page 33
Pattern Matching
