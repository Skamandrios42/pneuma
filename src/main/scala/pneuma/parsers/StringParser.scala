package pneuma.parsers

import Parser.Result.{Success, Failure}
import Parser.Result
import scala.util.matching.Regex

object StringParsers:

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

end StringParsers
