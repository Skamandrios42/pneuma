package pneuma.parsers

import scala.language.implicitConversions
import scala.io.StdIn
import org.scalatest.funsuite.AnyFunSuite
import Parser.Result.Success

class Tests extends AnyFunSuite:

    import StringParsers.{*, given}

    type DoubleParser = Parser[String, Unit, Double]

    def literal: DoubleParser = ("(" *> term.spaced <* ")") or ("-?[0-9]+(\\.[0-9]+)?".r map (_.toDouble))

    def factor: DoubleParser = literal.foldsep(("*" or "/").spaced) {
        case (a, op, b) => if op == "*" then a * b else a / b
    }

    def term: DoubleParser = factor.foldsep(("+" or "-").spaced) {
        case (a, op, b) => if op == "+" then a + b else a - b
    }
    def parse = term.spaced

    test("arithmetic parser") {
        assert(parse("3 + 5")._1 === Success(8.0))
        assert(parse("3 * (4 - 1)")._1 === Success(9.0))
    }
