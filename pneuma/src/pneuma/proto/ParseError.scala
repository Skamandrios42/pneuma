package pneuma.proto

import general.CompileError
import general.Metadata

object ParseError {
    given Ordering[ParseError] with
        def compare(a: ParseError, b: ParseError) = a.meta.start compare b.meta.start
}

case class ParseError(expected: String, found: String, meta: Metadata) extends CompileError {
    def name: String = "Syntax Error"
    def message: String = s"expected '$expected', but found '${found.takeWhile(_ != '\n')}'"
}