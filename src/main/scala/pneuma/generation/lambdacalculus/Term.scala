package pneuma.generation.lambdacalculus

enum Term {
  case Var(x: String)
  case Abs(x: String, body: Term)
  case App(abs: Term, arg: Term)
  
  override def toString = this match
    case Var(x) => x
    case Abs(x, body) => s"(\\$x.$body)"
    case App(abs, arg) => s"($abs $arg)"
  
}
