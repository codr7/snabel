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

### bindings

Values may be bound to identifiers once per scope using `let`, literals are bound at compile time.

```
  let foo 7

[]
  let foo 14

Error in 'repl', line 1, column 0:
Dup binding: foo
[]
  foo

[7]
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
  let foo (1 d 2 d 3)
  foo

[3]
```

### infix

Infix syntax may be triggered using `.`, the left hand side is shifted to first argument position on form expansion.

```
  foo.let 42
  foo

[42]
```

### conditions
`if` may be used to branch on a condition.

```
  if T 1 2
  
[1]
```

All values have a boolean representation; most are unconditionally `T`; but zero, empty stacks and strings etc. are considered `F`.

```
  if 0 1 2

[2]
```

### functions
New functions may be defined using `func`.

```
  func fib (n Int) (Int) if n.< 2 n + fib n.- 1 fib n.- 2
  fib 10

[]
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
  #(1 d 2 d 3)
  
[3]
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
Snabl v5
Press Return twice to evaluate.
May the source be with you!

  42 d
  
(TAGBODY
  ((VM-PUSH (CLONE 42))
   (UNLESS (DROP 1)
     (E-EMIT #S(POS :SOURCE repl :LINE 1 :COLUMN 3) Stack is empty))))
[]
  foo
  
; Evaluation aborted on #<SNABL::E-EMIT {1004285B63}>.
```