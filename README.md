# lambda-interpreter
Lambda calculus parser and interpreter with capture avoiding substitution. Evaluation by repeated beta-reduction of the leftmost, outermost redex with eta-conversion. Application is left associative and abstractions are right associative.

EBNF Grammar:
    
	Term := Variable | "\" . Abstraction | "(" . Application . ")" | "(" . Term . ")"
    
	Variable := < letters, numbers, symbols, etc. and can represent a constant, like "Y" >
    
	Application := Term . " " . Term . {" " . Term}
    
	Abstraction := Variable . "." . Term | Variable . " " . Abstraction


## Syntax examples:
- Y combinator: `(\f.((\x.(f (x x))) (\x.(f (x x)))))`

- S combinator: `(\x y z.(x z (y z)))`

- K combinator: `(\x y.x)`

- I combinator: `(\x.x)`

- if: `(\then else bool.(bool then else))`

## Examples:
- 2^10 with Church numerals

input:  `println(cnToInt(evalStr("(^ 10 2)")))`

output: `1024`

- SKK = I

input:  `println(evalStr("(S K K)"))`

output: `(\z.z)`


- YK\* = I

input: `println(evalStr("(Y K*)"))`

output: `(\y.y)`


- 4! with Church numerals

input: ((((\x.(\y.(y ((x x) y)))) (\x.(\y.(y ((x x) y))))) (\f.(\n.((((\n.((((\n.(\f.(\x.(((n (\g.(\h.(h (g f))))) (\u.x)) (\u.u))))) n) (\x.(\x.(\y.y)))) (\x.(\y.x)))) n) (\f.(\x.(f x)))) (((\m.(\n.((m ((\m.(\n.((m (\n.(\f.(\x.(f ((n f) x)))))) n))) n)) (\f.(\x.x))))) n) (f ((\n.(\f.(\x.(((n (\g.(\h.(h (g f))))) (\u.x)) (\u.u))))) n))))))) (\f.(\x.(f (f (f (f x)))))))

output: `(\f.(\x.(f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))))))))))`

