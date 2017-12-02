# lambda-interpreter
Lambda calculus parser and interpreter with capture avoiding substitution.

Grammar:
    
	Term := Variable | "(" . Application . ")" | "(" . Abstraction . ")" | "(" . Term . ")"
    
	Variable := < letters, numbers, symbols, etc. and can represent a constant, like "Y" >
    
	Application := Term . " " . Term
    
	Abstraction := Variable . " -> " . Term | Variable . " " . Abstraction


## Syntax examples:
- Y combinator: `(f -> ((x -> (f (x x))) (x -> (f (x x)))))`

- S combinator: `(x y z -> ((x z) (y z)))`

- K combinator: `(x y -> x)`

- I combinator: `(x -> x)`

- if: `(then else bool -> ((bool then) else))`

## Examples:
- 2^10 with Church numerals

input:  `println(cnToInt(evalStr("((^ 10) 2)")))`

output: `1024`

- SKK = I

input:  `println(evalStr("((S K) K)"))`

output: `(z -> z)`


- YK\* = I

input: `println(evalStr("(Y K*)"))`

output: `(y -> y)`


- (5 + 6) - 4 with Church numerals

input: `println(evalStr("(((m n -> ((n (n f x -> (((n (g h -> (h (g f)))) (u -> x)) (u -> u)))) m)) (((m n -> ((n (n f x -> (f ((n f) x)))) m)) (f x -> (f (f (f (f (f x))))))) (f x -> (f (f (f (f (f (f x))))))))) (f x -> (f (f (f (f x))))))"))`

output: `(f -> (x -> (f (f (f (f (f (f (f x)))))))))`


- 4! with Church numerals

input: `println(evalStr("(((f -> ((x -> (f (x x))) (x -> (f (x x))))) (f -> (n -> ((((then -> (else -> (bool -> ((bool then) else)))) (f -> (x -> (f x)))) (((m -> (n -> (f -> (m (n f))))) n) (f ((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n)))) ((n -> ((((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n) (x -> (x -> (y -> y)))) (x -> (y -> x)))) n))))) (f -> (x -> (f (f (f (f x)))))))"))`

output: `(f -> (x -> (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))))))))))`

- a scary one: fibonacci (fib_1 = 1, fib_2 = 1, fib_3 = 2, ...) with Church numerals

it is defined as: `"(theta (f n -> (((if 1) (((if 1) ((+ (f ((- n) 2))) (f (-- n)))) (isOne (-- n)))) (isOne n))))"` which means "if n is 1 or 2, return 1, else return fib(n - 2) + fib(n - 1). After a few reductions this becomes Fib(theta Fib) and the `f` argument copies the `theta Fib` function and applies it recursively, until it's at the tail position (n == 1 or n == 2) where it ends and stops the recursion by using the `if` functions. Leftmost derivation ensures that it is up to the leftmost redex to continue the recursion or not, and the theta / Y combinator won't go on looping forever.

(where theta is Turing's combinator, for recursivity, but you can also use the Y combinator)

which, when desugared, is actually:

`"(((x -> (y -> (y ((x x) y)))) (x -> (y -> (y ((x x) y))))) (f -> (n -> ((((then -> (else -> (bool -> ((bool then) else)))) (f -> (x -> (f x)))) ((((then -> (else -> (bool -> ((bool then) else)))) (f -> (x -> (f x)))) (((m -> (n -> ((n (n -> (f -> (x -> (f ((n f) x)))))) m))) (f (((m -> (n -> ((n (n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u)))))) m))) n) (f -> (x -> (f (f x))))))) (f ((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n)))) ((n -> ((((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n) (x -> (x -> (y -> y)))) (x -> (y -> x)))) ((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n)))) ((n -> ((((n -> (f -> (x -> (((n (g -> (h -> (h (g f))))) (u -> x)) (u -> u))))) n) (x -> (x -> (y -> y)))) (x -> (y -> x)))) n)))))"`

you can define this as being the constant "fib", and do this:

input: `println(evalStr("(fib 10)"))`

output: `(f -> (x -> (f (f ... (f x) ...))))`

...which has 55 f's => c_55, which represents the natural number 55 = fib_10.

