package pneuma.proto

import general.CompileError
import general.Region
import general.Pos

enum TypeError extends CompileError {
    case Mismatch(expected: Term, found: Term, region: Region = Region(None, 0, 0))
    case Unexpected(expected: String, region: Region = Region(None, 0, 0))
    case Undefined(variable: String, region: Region = Region(None, 0, 0))
    case NoImplicitFound(shape: Option[Term], region: Region = Region(None, 0, 0))
    case NoField(t: Term, field: String, region: Region = Region(None, 0, 0))
    case Message(msg: String, region: Region = Region(None, 0, 0))
    def name = "Type Error"
    def message: String = this match
        case Mismatch(expected, found, region) => s"expected '$expected', but found '$found'"
        case Unexpected(expected, region) => s"expected $expected"
        case Undefined(variable, region) => s"variable $variable is undefined"
        case NoImplicitFound(shape, region) => s"no implicit found for $shape"
        case NoField(t, field, region) => s"$t has no $field field"
        case Message(msg, region) => msg
    override def format(content: Seq[String]): String = toString
}