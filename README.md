## Snabl

### intro
Snabl is a concatenative language implemented and embedded in Common Lisp.

### setup
```
CL> (ql:quickload 'snabl)
CL> (in-package snabl)
SNABL> (let ((*vm* (new-vm))) 
         (lib-import (abc-lib *vm*))
         (repl))
Snabl v1
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