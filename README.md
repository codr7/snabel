## Snabl

### intro
Snabl is a scripting language implemented in Common Lisp.<br/>
<br/>
It is designed to be easy to customize and embed, and compiles to ordinary lambdas manipulating a global VM.

### setup
```
CL> (ql:quickload 'snabl)
CL> (in-package snabl)
SNABL> (let ((*vm* (new-vm))) 
         (lib-import *abc-lib*)
         (repl))
Snabl v3
Press Return twice to evaluate.
May the source be with you!

  42
  
[42]
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

Values may be bound to identifiers using `let`, literals are bound at compile time.

```
  let foo 42

[]
  foo

[42]
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

### inline Lisp

Lisp code may be embedded inline by prefixing with `$`.

```
  1 $(vm-push (new-val (int-type *abc-lib*) 2)) 3

[1 2 3]
```

### compile time evaluation

Compile time evaluation may be triggered by prefixing any form with `#`.

```
  #42
[42]
```