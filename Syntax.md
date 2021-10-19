# Syntax of _While_

This describes explains the textual representation of the programs written in _While_ language, by means of examples.

### Statements

The `skip` statement does nothing. In our lessons we needed this kind of statement whenever we wanted to have a loop or a conditional with an empty body. In this parser it is not necessary, since we can represent the absence of action with an empty block (see below). However, it is included in the parser, and you can feel free to use it, if needed.

```pascal
skip;
```

*Assignments* to variables are denoted by the `:=` operator:

```pascal
x := 4 + 6;
y := false;
```

This parser supports the following operators in descending precedence:

| Operator   | Comments                       |
| ---------- | ------------------------------ |
| `*`        | Left-associative               |
| `+`  `-`   | Left-associative               |
| `==`  `<=` | Non-associative                |
| `&&`       | Logical and. Right-associative |
| `\|\|`     | Logical or. Right-associative  |
| `?:`       | Conditional operator           |
| `:=`       | Assignment operator            |

For example, the expression `0 <= x ? x * 5 + 4 : y - 3` is equivalent to `(0 <= x) ? ((x * 5) + 4) : (y - 3)`. Parentheses can be used to alter precedence.

*Sequences* of statements are represented with the semicolon `;`. The last semicolon in a sequence of statements is optional. For example, the statement shown above is equivalent to the following one, where the last semicolon has been left out.

```pascal
x := 4 + 8;
y := false
```

We can write _if-then-else_ conditionals as follows:

```pascal
if 0 <= x then
	x := x + 5;
	y := 3;
else
	x := x - 5;
end
```

_Loops_ are written  as follows:

```pascal
while y <= x do
	y := y + 1;
	x := x - 1;
end;
y := 2 * x;
```

Notice the semicolon `;` after the `end`. It is necessary because the loop is followed by another statement. Otherwise it could be omitted, as in the `if` shown above.

### Local variables

The variables we have seen so far are global. However, one can define blocks with scoped variables as follows:

```pascal
x := 6;
y := 4;
begin
  var x := 0;
  var y := 5;
  z1 := x + y;
end;
z2 := x;
```

Here `z1` takes the value 5, whereas `z2` takes the value 6. 

The type of the local variables can be specified. For example:

```pascal
begin
	var x :: int := 5;
	var y :: bool := 0 <= x;
	skip;
end
```

### Function declarations

The following is an example of the function that returns the result of incrementing the value given by one.

```pascal
function increment(x) ret (y)
	y := x + 1;
end
z := increment(3);
```

Multiple parameters can be specified, but only one result is allowed. When the execution of the function finishes, the value contained within the result variable (`y` in the example above) is the value being returned.

For the sake of simplicity, function calls are only allowed in expressions of the form `x := f(e1, ..., en)`. They are allowed neither in subexpressions (as in `x := 3 + f(z)`)  nor in variable declarations (as in `begin var x := f(z); ... end`). 

We can define several functions. The general scheme is as follows:

```pascal
function f1(args1) ret (y1) 
  body1
end
function f2(args2) ret (y2) 
  body2
end...
function fn(args3) ret (y3) 
  body3
end
main_program
```

Note that:

* There is no semicolon after the `end` in the definition of a function.
* The _main program_ is mandatory.
* All the functions have to be defined before the first statement (i.e., you cannot interleave statements and function definitions).
* The types of the parameters and the result values can be specified with the `::` notation.
* If you want to define variables that are local to the function, you have to introduce them via scoped blocks with `begin` and `end`, as shown before.

Function definitions can be recursive:

```pascal
function fib(x :: int) ret (y :: int)
  if x <= 0 then
    y := 0
  else
    if x <= 1 then
      y := 1
    else
      begin
        var f1 := 0;
        var f2 := 0;
        f1 := fib(x - 1);
        f2 := fib(x - 2);
        y := f1 + f2;
      end
    end
  end
end

z := fib(4);
```

### Tuples

Tuples are sequences of elements. They are built with the syntax `(exp1, exp2, ..., expn)` and their components can be accessed via pattern matching: `(x1, x2, ..., xn) := exp`

```pascal
function dot_product(p1 :: (int, int), p2 :: (int, int)) ret (y :: int)
	begin
    	var p1x := 0;
        var p1y := 0;
        var p2x := 0;
        var p2y := 0;
        (p1x, p1y) := p1;
        (p2x, p2y) := p2;
        y := p1x * p2x + p1y * p2y;
    end
end
z := dot_product((4, 5), (1, 3));
```

Destructuring assignments cannot be used in local variable declarations. That is, the following is *not* allowed:

```pascal
function dot_product(p1 :: (int, int), p2 :: (int, int)) ret (y :: int)
	begin
    	var (p1x, p1y) := p1;  <== syntax error before '('
        var (p2x, p2y) := p2;
        y := p1x * p2x + p1y * p2y;
    end
end
z := dot_product((4, 5), (1, 3));
```

### Lists

Haskell-like functional lists are supported. The empty list is represented with `nil` and the list constructor is represented via `[_ | _]`.  For example, the following statement assigns the list [1,2,3] to the variable `xs`:

```pascal
xs := [1 | [2 | [3 | nil]]];
```

You can obtain the first element (head) of the list, and the remaining elements (tail) with the `hd` and `tl` postfix operators:

```pascal
y := xs.hd;
z := xs.tl;
```

In this case `y` would take the value `1` and `z` would take the value `[2 | [3 | nil]]`. 

Lists can be checked for emptiness with the `ifnil` construct:

```pascal
ifnil xs then
	... sequence of statements ...
else
	... sequence of statements ...
end
```

The following example shows how to implement a function that computes the length of a list:

```pascal
function length(xs :: [int]) ret (len :: int)
	ifnil xs then
    	len := 0;
    else
    	begin
        	var len_tail := 0;
            len_tail := length(xs.tl);
            len := 1 + len_tail;
        end
    end
end
z := length([1 | [2 | nil]]);
```

The type `[t]` represents the lists whose elements are of type `t`. 

The following example shows how to append two lists:

```pascal
function append(xs :: [int], ys :: [int]) ret (zs :: [int])
	ifnil xs then
    	zs := ys;
    else
    	begin
        	var concat_rec := nil;
            concat_rec := append(xs.tl, ys);
            zs := [xs.hd | concat_rec];
        end
    end
end
vs := concat([1 | [2 | nil]], [3 | [4 | [5 | nil]]]);
```

