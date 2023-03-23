package pneuma.proto

import general.CompileError
import general.Region

case class ParseError(expected: String, found: String, region: Region) extends CompileError {
    def name: String = "Syntax Error"
    def message: String = s"expected '$expected', but found '$found'"
}