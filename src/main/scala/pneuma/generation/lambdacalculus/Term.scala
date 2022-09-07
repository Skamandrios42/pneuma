package pneuma.generation.lambdacalculus

import scala.sys.process.Process

enum Term {
  case Var(x: String)
  case Abs(x: String, body: Term)
  case App(abs: Term, arg: Term)
  case Str(value: String)
  case Print(t: Term)

  override def toString = this match
    case Var(x)        => x
    case Abs(x, body)  => s"(\\$x.$body)"
    case App(abs, arg) => s"($abs $arg)"
    case Str(value)    => s"\"$value\""
    case Print(t)      => s"(print $t)"
}
@main 
def test() =
  val input = """(\msg . print ( (\f . \x . f (f (f x))) (\a . print a) msg ) ) "Hey" """
  for term <- Parser(input) do
    println(term)
    Generator("Calculus", term)
  println(Process("java Calculus").run().exitValue())