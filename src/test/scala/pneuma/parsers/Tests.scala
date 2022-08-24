package pneuma.parsers

import scala.language.implicitConversions
import scala.io.StdIn
import org.scalatest.funsuite.AnyFunSuite
import Parser.Result.{Success, Failure}

class Tests extends AnyFunSuite:

    import StringParsers.{*, given}

    type DoubleParser = Parser[(String, Int), StringError, Double]
    
    val number  = "-?[0-9]+(\\.[0-9]+)?".r.transform(_.toDouble, _.copy(expected = "number"))

    val literal = "(" *> term.spaced <* ")" or number

    val factor  = literal.foldsep(("*" or "/").spaced) {
        case (a, op, b) => if op == "*" then a * b else a / b
    }

    val term: DoubleParser = factor.foldsep(("+" or "-").spaced) {
        case (a, op, b) => if op == "+" then a + b else a - b
    }



    val parse = term.spaced(_: String, 0)

    def debug = parse(_: String) match
        case (Success(value), _) => println(value)
        case (Failure(error), _) => println(error)

    test("error messages") {
        println(("H" <*> "E")("Hello World!", 0))
        debug("2 +")
        debug("3 * (2 + 2]")
    }

    test("arithmetic parser") {
        assert(parse("3 + 5")._1 === Success(8.0))
        assert(parse("3 * (4 * 2 - 1)")._1 === Success(21.0))
    }
