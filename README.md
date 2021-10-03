## Snabl

[![Liberapay](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/andreas7/donate)

### intro
Snabl is a scripting language implemented in Common Lisp.<br/>
<br/>
It's designed to be easy to customize and embed, and compiles to ordinary lambdas manipulating a global VM.

### setup
```
CL> (ql:quickload 'snabl)
CL> (in-package snabl)
SNABL> (let ((*vm* (new-vm))) 
         (lib-import *abc-lib*)
	 (lib-import *math-lib*)
         (repl))
Snabl v6
Press Return twice to evaluate.
May the source be with you!

  func fib (n Int) (Int) if n.< 2 n + fib n.- 1 fib n.- 2
  fib 10

[55]
```

### bindings

Values may be bound to identifiers once per scope using `let`, literals are bound at compile time.

```
  let foo 42

[]
  let foo 42

Error in 'repl', line 1, column 0:
Dup binding: foo
[]
```

`_` may be used as a placeholder to get the top value from the stack.

```
  42

[42]
  let foo _

[]
  foo

[42]
```

### scopes
Braces may be used to create child scopes.

```
  {let foo 42}
  foo

Error in 'repl', line 2, column 0:
Unknown id: foo
[]
```

### groups
Parens may be used to group forms.

```
  let foo (dump 'binding 42)
  foo

'binding
[42]
```

### infix

Infix syntax may be triggered using `.`, the left hand side is shifted to first argument position on form expansion.

```
  foo.let 42
  foo

[42]
```

### branching
`if` may be used to branch on a condition.

```
  if T 'yes 'no
  
['yes]
```

All values have a boolean representation; most are unconditionally `T`; but zero, empty stacks and strings etc. are considered `F`.

```
  if 0 'yes 'no

['no]
```

### functions
New functions may be defined using `func`.

```
  func fib (n Int) (Int) if n.< 2 n + fib n.- 1 fib n.- 2
  fib 10

[55]
```

### call flags
Function calls may specify flags immediately following the target.

#### --drop | -d
Drops returned values as soon as possible during the call.

```
  func foo () (Int) 42

[]

  foo

[42]

  d

[]

  foo --drop

[]
```

#### --tco | -t
Reuses the existing call frame instead of pushing a new one.

#### --unsafe | -u
Skips type checks on arguments and return values.

### quoting
Any expression may be quoted by prefixing with `'`.

Literals evaluate to themselves.
```
  '42
[42]
```

Identifiers become symbols.
```
  'foo
['foo]
```

### the stack

Values are automatically pushed on the stack.

```
  1 2 3

[1 2 3]
```

`d` may be used to drop values.

```
  1 2 3 4 5 ddd
  
[1 2]
```

`cp` may be used to copy the top value.

```
  1 2 3 cp
  
[1 2 3 3]
```

### inline Lisp

Lisp code may be embedded inline by prefixing with `$`.

```
  1 $(vm-push (new-val (int-type *abc-lib*) 2)) 3

[1 2 3]
```

### compile time evaluation

Compile time evaluation may be triggered by prefixing any form with `#`.

```
  func foo () (Int) #(dump 'eval 42)

'eval
[]
  foo

[42]
```

### debugging

`dump` may be used to dump any value to `cl:*standard-output*`.

```
  dump 42

42
[]
```

Setting `snabl:*debug*` to `T` prevents the REPL from trapping conditions and dumps generated Lisp code to `stdout`.

```
SNABL> (let ((*vm* (new-vm))
             (*debug* t)) 
         (lib-import *abc-lib*)
	 (lib-import *math-lib*)
         (repl))
  42 d

((VM-PUSH (THE VAL (CLONE 42)))
     (UNLESS (DROP (THE INTEGER 1))
       (E-EMIT in 'repl' on line 1, column 3 Stack is empty)))
[]
  foo
  
; Evaluation aborted on #<SNABL::E-EMIT {1004285B63}>.
```

### performance

The following snippet repeats the classical recursive fibonacci(20) a hundred times and measures the elapsed time in ms.

```
func fibr (n Int) (Int) 
  if n.< 2 n + fibr n.- 1 fibr n.- 2
bench 100 fibr -d -u 20

[2586]
```

Fibonacci again, but now with a tail recursive algoritm; running fibonacci(70) ten thousand times.

```
func fibt (n Int a Int b Int) (Int)
  if n.is 0 a if n.is 1 b fibt -t n.- 1 b a.+ b
bench 10000 fibt -d -u 70 0 1

[592]
```