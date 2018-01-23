# Rho

###Proc

Generalized Abstract Data Type for the Rho-Calculus

####AbstractInterpreter

Includes a monad `Trace` for logging state changes over an expression evaluation, and an interface `RhoInterface` for evaluating expressions written with the Rho ADT. 

A number of sample expressions exist in the `Example` object. To run the interpreter, call `Example.evaluate` on an expression, then open `SBT Shell` and type `run`.

