package pneuma.parsers

import Parser.Result.{Success, Failure}
import Parser.Result
import scala.util.matching.Regex
import languageFeature.implicitConversions

object StringParsers:

    case class StringError(expected: String, found: String) {
        override def toString = s"expected '$expected', but found '$found'"
    }

    def str(s: String): Parser[String, StringError, String] =
        (src: String) => if src.startsWith(s)
            then (Success(s), src.drop(s.length))
            else (Failure(StringError(s, src.take(10))), src)

    def regex(r: Regex): Parser[String, StringError, String] =
        (src: String) => r.findPrefixOf(src) match
            case Some(s) => (Success(s), src.drop(s.length))
            case None    => (Failure(StringError(r.toString, src.take(10))), src)

    given Conversion[String, Parser[String, StringError, String]] = str(_)
    given Conversion[Regex, Parser[String, StringError, String]] = regex(_)

    def skip = regex(" *".r)

    extension [E, A](self: Parser[String, E, A]) def spaced = skip *> self <* skip

end StringParsers
