package pneuma.core

enum TypeError {
    case Mismatch(expected: Term, found: Term)
}

enum Pattern {
    case Var
    case Cons(fields: List[Pattern])

    def context(arg: Term): Map[Int, Term] = ???
}

enum Mode {
    case Normal
    case Inferred
    case Implicit
}

case class Cons(inferred: List[Term], indices: List[Term], fields: List[Term])

enum Term {

    case Var(index: Int)

    case Abs(cases: List[(Pattern, Term)], mode: Mode)
    case Pro(in: Term, out: Term, mode: Mode)
    case App(function: Term, arg: Term, mode: Mode)

    case Module(named: Map[String, Term], typed: Map[Term, Term])
    case Interface(named: Map[String, Term], typed: Set[Term])

    case Data(indexTypes: List[Term], constructors: List[Cons])

    def shift(amount: Int, cutoff: Int): Term = ???

    def replace(index: Int, term: Term): Term = ???

    def eval: Term = ???
        
    def eq(that: Term, relation: Set[(Term, Term)]): Boolean = ???

    def check(typ: Term, context: Map[Int, Term], proofs: Map[Term, Term]): Term = ???

    override def toString: String = "<term>"

}