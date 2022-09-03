package pneuma.generation.lambdacalculus

import scala.language.implicitConversions

import pneuma.parsers.StringParsers
import pneuma.parsers.Parser
import pneuma.parsers.Parser.Result
import pneuma.generation.lambdacalculus.Term.{Var, Abs, App}

object Parser {

    import StringParsers.{*, given}

    type LParser = Parser[(String, Int), StringError, Term]

    val ident = regex("[_a-zA-Z][_a-zA-Z0-9]*".r).transform(identity, _.copy(expected = "identifier"))
    
    val variable = ident.map(Var(_)) or ("(" *> application.spaced <* ")")
    
    val abstraction = for 
        _    <- str("\\")
        x    <- ident.spaced
        _    <- str(".").spaced
        body <- application
    yield Abs(x, body)

    val expr = abstraction or variable

    val application: LParser = (expr <* skip).fold(App(_, _))

    val parse = application.spaced(_: String, 0)

}

@main def test() =
    val term = Parser.parse("\\f .  \\x . f (f (f x))")
    println(term)
    term match
        case (Result.Success(value), _) => Generator("Calculus", value)
        case (Result.Failure(value), _) => println("no generation...")
