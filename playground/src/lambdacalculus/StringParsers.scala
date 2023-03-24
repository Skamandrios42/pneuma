package lambdacalculus

import general.Result.{Success, Failure}
import general.Result
import parsing.Parser
import scala.util.matching.Regex
import languageFeature.implicitConversions

object StringParsers {
    case class StringError(expected: String, found: String, pos: Int) {
        override def toString = s"[$pos] expected '$expected', but found '$found'"
    }

    def str(s: String): Parser[(String, Int), StringError, String] =
        (src, pos) => if src.startsWith(s)
            then (Success(s), (src.drop(s.length), pos + s.length))
            else (Result.fail(StringError(s, src.take(10), pos)), (src, pos))

    def regex(r: Regex): Parser[(String, Int), StringError, String] =
        (src, pos) => r.findPrefixOf(src) match
            case Some(s) => (Success(s), (src.drop(s.length), pos + s.length))
            case None    => (Result.fail(StringError(r.toString, src.take(10), pos)), (src, pos))

    given Conversion[String, Parser[(String, Int), StringError, String]] = str(_)
    given Conversion[Regex, Parser[(String, Int), StringError, String]] = regex(_)

    def skip = regex("\\s*".r)

    extension [E, A](self: Parser[(String, Int), E, A]) def spaced = skip *> self <* skip

}
