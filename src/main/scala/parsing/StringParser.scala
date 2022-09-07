package parsing

import Parser.Result.{Success, Failure}
import Parser.Result
import scala.util.matching.Regex
import languageFeature.implicitConversions

object StringParsers {
    given Ordering[StringError] with
        def compare(a: StringError, b: StringError) = a.pos compare b.pos

    case class StringError(expected: String, found: String, pos: Int) {
        override def toString = s"[$pos] expected '$expected', but found '$found'"
    }

    def str(s: String): Parser[(String, Int), StringError, String] =
        (src, pos) => if src.startsWith(s)
            then (Success(s), (src.drop(s.length), pos + s.length))
            else (Failure(StringError(s, src.take(10), pos)), (src, pos))

    def regex(r: Regex): Parser[(String, Int), StringError, String] =
        (src, pos) => r.findPrefixOf(src) match
            case Some(s) => (Success(s), (src.drop(s.length), pos + s.length))
            case None    => (Failure(StringError(r.toString, src.take(10), pos)), (src, pos))

    given Conversion[String, Parser[(String, Int), StringError, String]] = str(_)
    given Conversion[Regex, Parser[(String, Int), StringError, String]] = regex(_)

    def skip = regex(" *".r)

    extension [E, A](self: Parser[(String, Int), E, A]) def spaced = skip *> self <* skip

}
