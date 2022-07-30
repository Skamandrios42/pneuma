package pneuma.parsers

import scala.util.matching.Regex
import Result.{Success, Failure}
import scala.io.StdIn

enum Result[+E, +A]:
    case Success(value: A)
    case Failure(value: E)

object Parser {

    def from[S, A](f: S => (A, S)): Parser[S, Nothing, A] = (src0: S) =>
      val (result, src1) = f(src0)
      (Success(result), src1)

    def apply[S] = new Creator[S]

    class Creator[S]:
      def success[A](value: => A): Parser[S, Nothing, A] = (src: S) => (Success(value), src)
      def failure[E](value: => E): Parser[S, E, Nothing] = (src: S) => (Failure(value), src)
      def result[E, A](value: => Result[E, A]): Parser[S, E, A] = (src: S) => (value, src)

}

trait Parser[S, +E, +A] extends (S => (Result[E, A], S)) {

    def map[B](f: A => B): Parser[S, E, B] =
        this(_) match
            case (Success(value), src) => (Success(f(value)), src)
            case (Failure(value), src) => (Failure(value), src)

    def flatMap[B, F](f: A => Parser[S, F, B]): Parser[S, E | F, B] =
        this(_) match
            case (Success(value), src) => f(value)(src)
            case (Failure(value), src) => (Failure(value), src)

    def <*>[F, B](that: => Parser[S, F, B]): Parser[S, E | F, (A, B)] =
        for a <- this
            b <- that
        yield (a, b)

    def *>[F, B](that: => Parser[S, F, B]): Parser[S, E | F, B] =
        for _ <- this
            b <- that
        yield b

    def <*[F, B](that: => Parser[S, F, B]): Parser[S, E | F, A] =
        for a <- this
            _ <- that
        yield a

    def or[F, B](that: => Parser[S, F, B]): Parser[S, F, A | B] = (src: S) =>
        this(src) match
            case (Success(value), src) => (Success(value), src)
            case (Failure(value), _)   => that(src)

    def rep: Parser[S, Nothing, List[A]] = (
        for head <- this
            tail <- rep
        yield head :: tail) or Parser[S].success(List.empty)

    def fold[A1 >: A](op: (A1, A1) => A1): Parser[S, E, A1] =
        for head <- this
            tail <- this.rep
        yield tail.foldLeft(head)(op)

    def foldsep[F, B, A1 >: A](that: Parser[S, F, B])(op: (A1, B, A1) => A1): Parser[S, E, A1] =
        for head <- this
            tail <- (that <*> this).rep
        yield tail.foldLeft[A1](head) { case (a0, (b, a1)) => op(a0, b, a1)}

}

object ParserUtils:

    import languageFeature.implicitConversions

    def str(s: String): Parser[String, Unit, String] =
        (src: String) => if src.startsWith(s)
            then (Success(s), src.drop(s.length))
            else (Failure(()), src)

    def regex(r: Regex): Parser[String, Unit, String] =
        (src: String) => r.findPrefixOf(src) match
            case Some(s) => (Success(s), src.drop(s.length))
            case None    => (Failure(()), src)

    given Conversion[String, Parser[String, Unit, String]] = str(_)
    given Conversion[Regex, Parser[String, Unit, String]] = regex(_)

    def skip = regex(" *".r)

    extension [E, A](self: Parser[String, E, A]) def spaced = skip *> self <* skip





object Arith:

    import ParserUtils.{*, given}

    type IntParser = Parser[String, Unit, Int]

    def literal: IntParser = ("(" *> term.spaced <* ")") or ("-?[0-9]+(\\.[0-9]+)?".r map (_.toInt))

    def factor: IntParser = literal.foldsep(("*" or "/").spaced) {
        case (a, op, b) => if op == "*" then a * b else a / b
    }

    def term: IntParser = factor.foldsep(("+" or "-").spaced) {
        case (a, op, b) => if op == "+" then a + b else a - b
    }

    def parse(input: String) = term.spaced(input) match
        case (Success(value), src) if src.isEmpty => Some(value)
        case _ => None



@main
def arithmetic(): Unit =
    while true do
        print("> ")
        val input = StdIn.readLine()
        if input == "exit" then return
        else println(Arith.parse(input))

@main
def testParsers(): Unit =
    
    def str(s: String): Parser[String, Unit, String] =
        (src: String) => if src.startsWith(s)
            then (Success(s), src.drop(s.length))
            else (Failure(()), src)

    def regex(r: Regex): Parser[String, Unit, String] =
        (src: String) => r.findPrefixOf(src) match
            case Some(s) => (Success(s), src.drop(s.length))
            case None    => (Failure(()), src)

    val myParser = for
        hello <- str("Hello")
        _     <- regex(" *".r)
        world <- str("World!")
    yield s"$hello $world"

    println(str("A").rep(""))
    println((str("a") or str("b"))(""))
    println(((str("a") or str("b")) <*> str("c")).rep("acbcbcaca"))

    println(myParser("Hello World! more text"))
    println(myParser("Hello    World!"))
    println(myParser("HelloWorld!"))
    println(myParser("Hello World"))
    println(myParser("Helo World"))
    println(Parser[String].success(42) apply "Rest")
    println(Parser[String].failure(23) apply "Rest")
    println(Parser[String].result(Success(42)) apply "Rest")

@main
def run(): Unit = println("Hello World!")
