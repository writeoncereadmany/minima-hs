## Minima-Lang

Minima is a very small programming language, intended as a basis on which to build language features quickly.

There are four principal concepts in Minima:

### 1: Variables

A variable in Minima is a name for a value. It exists only within the scope in which it's defined.
Variables can only be declared - not reassigned. This is done using the `is` keyword (the only keyword in Minima).

```
message is "Hello, World!"
```

Declaring a variable is an expression which always returns `SUCCESS`.

Whenever a variable is included in an expression, it is replaced with whatever value it was declared with.

### 2: Functions

A function in Minima is defined as the combination of variables and a body (which is an expression).
When a function is called, the variables are assigned the call's arguments, and then the expression is
evaluated.

Functions do not have names. Instead of building named functions, we build function expressions and assign them
to variables.

Functions use square brackets `[]` to define a comma-separated variable/argument list,
and an arrow `=>` to connect a variable list to its body.

For example, this is the `double` function:

```
double is [x] => x:multiplyBy[2]
```

And this is how we call it:

```
double[23]
```

### 3: Groups

Whilst there is no capability for updating variables, functions can cause side-effects like printing. In addition,
sometimes it is desirable to declare variables inside function bodies. As a function is defined as having a single
expression as a body, it is therefore necessary to group a sequence of functions together.

A group is a parenthesised, comma separated list of expressions. When evaluated, it evaluates each expression in
turn and returns the value of the last expression evaluated.

For example, this declares a function and then calls it:

```
sixteen is (
  square is [x] => x:multiplyBy[x],
  square[4]
)
```

Note that a group with one entry is effectively a parenthesised expression.

### 4: Objects

An object is a mapping of names to values. An object is defined using curly braces `{}`, and contains a comma-separated
list of colon-separated pairs.

A field of an object can be accessed using the colon `:` operator.

```
point is { x : 2, y : 3 }
xCoord is point:x
```

### 5: Literals

In addition to the above structures, two data-types have literal syntax built into the language:
  * Numbers (double-precision floating-point), in either integral or decimal form
  * Strings, delimited by double-quotes

```
anInteger is 42
aDecimal is 3.50
aString is "no one"
```