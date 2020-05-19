### Proc

Contains a GADT for the &rho;-calculus

### Trace
Includes a monad transformer `Trace` for logging state changes over an expression evaluation.

### Interpreter
An interface for evaluating expressions written using the Rho ADT. A number of sample expressions exist in the `Example` object. To run the interpreter, call `Example.evaluate(P)` on an expression, `P`, then open `SBT Shell` and type `run`.

