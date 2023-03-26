package pneuma.proto

import general.CompileError
import general.Region

object ParseError {
    given Ordering[ParseError] with
        def compare(a: ParseError, b: ParseError) = a.region.start compare b.region.start
}

case class ParseError(expected: String, found: String, region: Region) extends CompileError {
    def name: String = "Syntax Error"
    def message: String = s"expected '$expected', but found '$found'"
    def format(content: Seq[String]): String = toString
}