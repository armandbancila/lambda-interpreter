import scala.language.implicitConversions
import scala.language.reflectiveCalls

// basic parser
abstract class Parser[I <% Seq[_], T] {
  def parse(ts: I): Set[(T, I)]

  def parse_all(ts: I): Set[T] =
    for ((head, tail) <- parse(ts); if (tail.isEmpty)) yield head
}

// apply two parsers in sequence
class SeqParser[I <% Seq[_], T, S](p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, (T, S)] {
  def parse(string: I) =
    for ((head1, tail1) <- p.parse(string);
         (head2, tail2) <- q.parse(tail1)) yield ((head1, head2), tail2)
}

// apply both parsers and do a set union on their separate outputs
class AltParser[I <% Seq[_], T](p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
  def parse(string: I) = p.parse(string) ++ q.parse(string)
}

// apply a function to a parser's output
class FunParser[I <% Seq[_], T, S](p: => Parser[I, T], f: T => S) extends Parser[I, S] {
  def parse(string: I) =
    for ((head, tail) <- p.parse(string)) yield (f(head), tail)
}

// implicit conversion stuff
case class StringParser(s: String) extends Parser[String, String] {
  def parse(sb: String) = {
    val (prefix, suffix) = sb.splitAt(s.length)
    if (prefix == s) Set((prefix, suffix)) else Set()
  }
}

// auto convert a String to a parser if required by the context
implicit def string2parser(s : String) = StringParser(s)

// define new operators that create parsers
implicit def parserOps[I<% Seq[_], T](p: Parser[I, T]) = new {
  def || (q: => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q: => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

// do the same for strings and parsers
implicit def StringOps(s: String) = new {
  def || (q : => Parser[String, String]) = new AltParser[String, String](s, q)
  def || (r: String) = new AltParser[String, String](s, r)
  def ==>[S] (f: => String => S) = new FunParser[String, String, S](s, f)
  def ~[S](q : => Parser[String, S]) = 
    new SeqParser[String, String, S](s, q)
  def ~ (r: String) = 
    new SeqParser[String, String, String](s, r)
}

// a term can be a variable, an application of 2 terms, or an abstraction = function
abstract class Term
case class Var(name: String) extends Term
case class App(function: Term, argument: Term) extends Term
case class Abs(argument: Term, image: Term) extends Term

var constants: Map[String, Term] = Map()

case object NameParser extends Parser[String, String] {
  val reg = "[a-zA-Z0-9!$%^&*_+='#~<>./?|-]*".r // allowed variable names
  def parse(sb: String) = reg.findPrefixOf(sb) match {
    case None => Set()
    case Some(s) => Set(sb.splitAt(s.length))
  }
}

// variables can be simple names or they can represent constants
lazy val VarParser: Parser[String, Term] =
  NameParser ==> { // replace variable with the term it represents, if defined
    case x => {
      if (constants contains x) constants(x)
      else Var(x)
    }
  }

// abstractions
lazy val AbsParser: Parser[String, Term] =
  VarParser ~ " -> " ~ Term ==> { case ((a, b), c) => Abs(a, c): Term } ||
  VarParser ~ " " ~ AbsParser ==> { case ((a, b), c) => Abs(a, c) }

// applications
lazy val AppParser: Parser[String, Term] =
  (Term ~ " " ~ Term ==> { case ((a, b), c) => App(a, c): Term })

// terms
lazy val Term: Parser[String, Term] =
  VarParser ||
  (("(" ~ AppParser ~ ")") ==> { case ((a, b), c) => b }) ||
  (("(" ~ AbsParser ~ ")") ==> { case ((a, b), c) => b }) ||
  (("(" ~ Term ~ ")") ==> { case ((a, b), c) => b })

// list the unbound variables in a lambda term
def freeVars(term: Term): Set[String] = term match {
  case Var(name) => Set(name)
  case App(function, argument) => freeVars(function) ++ freeVars(argument)
  case Abs(Var(name), image) => freeVars(image) - name
}

// capture avoiding substitution
// so that alpha-conversion isn't needed
def substitute(argument: Term, image: Term, term: Term): Term = image match {
  case Var(name) => {
    if (argument == image) term
    else image
  }
  case App(a, b) => App(substitute(argument, a, term), substitute(argument, b, term))
  case Abs(Var(a), b) => {
    if (Var(a) == argument) image
    else if ((Var(a) != argument) && (freeVars(term) contains a)) {
      var v = a + "'"
      while ((freeVars(b) contains v) && (freeVars(term) contains v)) v = v + "'"
      Abs(Var(v), substitute(argument, substitute(Var(a), b, Var(v)), term))
    }
    else Abs(Var(a), substitute(argument, b, term))
  }
}

// contract a redex
def betaReduce(term: Term): Term = term match {
  case App(Abs(a, b), c) => substitute(a, b, c)
  case App(a, b) => {
    val contract = App(betaReduce(a), b)
    if (contract != term) contract
    else App(a, betaReduce(b))
  }
  case Abs(a, b) => Abs(a, betaReduce(b))
  case _ => term
}

// simplifies terms of the form (x -> (f x)) to f, if x isn't in f
def etaConvert(term: Term): Term = term match {
  case Abs(Var(x), App(f, Var(y))) if (!(freeVars(f) contains x) && x == y) => f
  case _ => term
}

// repeatedly apply beta-reduction and eta-conversion
// until a normal form is reached (if it exists)
// leftmost conversion will always reach a nf, if it exists
def eval(input: Term): Term = {
  val betaReduct = betaReduce(input)
  if (betaReduct != input) eval(betaReduct)
  else {
    val convert = etaConvert(input)
    if (convert != input) eval(convert)
    else input
  }
}

def termToStr(term: Term): String = term match {
  case Var(a) => a
  case App(a, b) => "(" + termToStr(a) + " " + termToStr(b) + ")"
  case Abs(a, b) => "(" + termToStr(a) + " -> " + termToStr(b) + ")"
}

// parse a string representation of a term into a Term
def lambdaParse(input: String): Term = Term.parse_all(input).head

// evaluate a term in a string by parsing & beta-reducing it repeatedly
def evalStr(input: String): String = termToStr(eval(lambdaParse(input)))

// convert Church numerals to Int
def cnToInt(term: Term): Int = term match {
  case App(Var(a), Var(b)) => 1
  case App(a, b) => 1 + cnToInt(b)
  case Abs(a, Abs(b, c)) => cnToInt(c)
}

// define constants for the language
// constants such as the Y combinator
constants = constants ++ Map(
  ("S" -> lambdaParse("(x y z -> ((x z) (y z)))")),
  ("K" -> lambdaParse("(x y -> x)")),
  ("I" -> lambdaParse("(x -> x)")),
  ("K*" -> lambdaParse("(x y -> y)")),
  ("Y" -> lambdaParse("(f -> ((x -> (f (x x))) (x -> (f (x x)))))")), // Y combinator
  ("theta" -> lambdaParse("((x y -> (y ((x x) y))) (x y -> (y ((x x) y))))")) // Turing's fixed point combinator
)

constants += "true" -> lambdaParse("K")
constants += "false" -> lambdaParse("K*")
constants += "if" -> lambdaParse("(then else bool -> ((bool then) else))")
constants += "0" -> lambdaParse("(f x -> x)")
constants += "1" -> lambdaParse("(f x -> (f x))")
constants += "2" -> lambdaParse("(f x -> (f (f x)))")
constants += "3" -> lambdaParse("(f x -> (f (f (f x))))")
constants += "4" -> lambdaParse("(f x -> (f (f (f (f x)))))")
constants += "5" -> lambdaParse("(f x -> (f (f (f (f (f x))))))")
constants += "++" ->  lambdaParse("(n f x -> (f ((n f) x)))")
constants += "--" ->  lambdaParse("(n f x -> (((n (g h -> (h (g f)))) (u -> x)) (u -> u)))")
constants += "+" ->  lambdaParse("(m n -> ((n ++) m))")
constants += "-" ->  lambdaParse("(m n -> ((n --) m))")
constants += "*" -> lambdaParse("(m n f -> (m (n f)))")
constants += "^" -> lambdaParse("(m n -> (n m))")
constants += "isZero" -> lambdaParse("(n -> ((n (x -> false)) true))")
constants += "isOne" -> lambdaParse("(n -> (((-- n) (x -> false)) true))")
constants += "fac" -> lambdaParse("(Y (f n -> (((if 1) ((* n) (f (-- n)))) (isOne n))))")
constants += "10" -> eval(lambdaParse("((* 5) 2)"))

println("> solution F to (F I) = x, (F K) = y")
constants += "F" -> lambdaParse("(a -> ((((a K) I) y) x))")
println(evalStr("(F I)"))
println(evalStr("(F K)"))
println("> leftmost derivation avoids infinite loop and finds nf")
println(evalStr("(Y K*)"))
println("> SKK = I")
println(evalStr("((S K) K)"))
println("> Church numeral arithmetic, c_15 - c_4 = c_11")
println(evalStr("((- ((+ 5) 10)) 4)"))
println("> 4!")
println((evalStr(("(fac 4)"))))

