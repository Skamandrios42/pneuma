

import scala.util.matching.Regex
import general.Result.{Success, Failure}
import scala.io.StdIn
import general.Result

@main
def run(): Unit =

    def str(s: String): Parser[String, Unit, String] =
        (src: String) => if src.startsWith(s)
            then (Success(s), src.drop(s.length))
            else (Result.fail, src)

    def regex(r: Regex): Parser[String, Unit, String] =
        (src: String) => r.findPrefixOf(src) match
            case Some(s) => (Success(s), src.drop(s.length))
            case None    => (Result.fail, src)

    val myParser = for
        hello <- str("Hello")
        _     <- regex(" *".r)
        world <- str("World!")
    yield s"$hello $world"

    println(str("A").repeat(""))
    println((str("a") or str("b"))(""))
    println(((str("a") or str("b")) <*> str("c")).repeat("acbcbcaca"))

    println(myParser("Hello World! more text"))
    println(myParser("Hello    World!"))
    println(myParser("HelloWorld!"))
    println(myParser("Hello World"))
    println(myParser("Helo World"))
    println(Parser[String].success(42) apply "Rest")
    println(Parser[String].failure(23) apply "Rest")
    println(Parser[String].result(Success(42)) apply "Rest")
