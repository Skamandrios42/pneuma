package pneuma.generation.lambdacalculus

import scala.language.implicitConversions

import pneuma.parsers.StringParsers
import pneuma.parsers.Parser
import pneuma.parsers.Parser.Result
import pneuma.generation.lambdacalculus.Term.{Var, Abs, App, Str, Print}

object Parser {

    import StringParsers.{*, given}

    type LParser = Parser[(String, Int), StringError, Term]

    val ident = regex("[_a-zA-Z][_a-zA-Z0-9]*".r).transform(identity, _.copy(expected = "identifier"))
    
    val string = regex("\"([^\"\\\\]|\\\\[\\s\\S])*\"".r).transform(
        str => Str(str.substring(1, str.length - 1)), 
        err => err.copy(expected = "string-literal")
    )

    val variable = ident.map(Var(_)) or ("(" *> application.spaced <* ")")
    
    val abstraction = for 
        _    <- str("\\")
        x    <- ident.spaced
        _    <- str(".").spaced
        body <- application
    yield Abs(x, body)

    val expr = abstraction or variable or string

    val application: Parser[(String, Int), StringError, Term] = (expr <* skip).fold {
        case (Var("print"), arg) => Print(arg)
        case (abs, arg) => App(abs, arg)
    }

    def apply(input: String): Result[StringError, Term] = application.spaced(input, 0)._1

}
