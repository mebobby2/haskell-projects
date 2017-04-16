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

# Upto

Page 29
Returning More than One Value
