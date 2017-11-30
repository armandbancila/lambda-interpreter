# lambda-interpreter
Lambda calculus parser and interpreter.

## Syntax examples:
- Y combinator: `(f -> ((x -> (f (x x))) (x -> (f (x x)))))`

- S combinator: `(x y z -> ((x z) (y z)))`

- K combinator: `(x y -> x)`

- I combinator: `(x -> x)`

- if: `(then else bool -> ((bool then) else))`

## Examples:
- SKK = I

input:  `println(evalStr("((S K) K)"))`

output: `(z -> z)`


- YK\* = I

input: `println(evalStr("(Y K\*)"))`

output: `(y -> y)`


- (5 + 6) - 4 with Church numerals

input: `println(evalStr("(((m n -> ((n (n f x -> (((n (g h -> (h (g f)))) (u -> x)) (u -> u)))) m)) (((m n -> ((n (n f x -> (f ((n f) x)))) m)) (f x -> (f (f (f (f (f x))))))) (f x -> (f (f (f (f (f (f x))))))))) (f x -> (f (f (f (f x))))))"))`

output: `(f -> (x -> (f (f (f (f (f (f (f x)))))))))`


- 4! with Church numerals

input: `println(evalStr("(((f -> ((x -> (f (x x))) (x -> (f (x x))))) (f -> (n -> ((((then -> (else -> (bool -> ((bool then) else)))) (f -> (x -> (f x)))) (((m -> (n -> (f -> (m (n f))))) n) (f ((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n)))) ((n -> ((((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n) (x -> (x -> (y -> y)))) (x -> (y -> x)))) n))))) (f -> (x -> (f (f (f (f x)))))))"))`

output: `(f -> (x -> (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))))))))))`

