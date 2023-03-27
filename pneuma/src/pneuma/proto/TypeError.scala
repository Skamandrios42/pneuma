package pneuma.proto

import general.CompileError
import general.Region
import general.Pos

enum TypeError extends CompileError {
    case Mismatch(expected: Term, found: Term, region: Region)
    case Unexpected(expected: String, region: Region)
    case Undefined(variable: String, region: Region)
    case NoImplicitFound(shape: Option[Term], region: Region)
    case NoField(t: Term, field: String, region: Region)
    case Message(msg: String, region: Region)
    def name = "Type Error"
    def message: String = this match
        case Mismatch(expected, found, region) => s"expected '$expected', but found '$found'"
        case Unexpected(expected, region) => s"expected $expected"
        case Undefined(variable, region) => s"variable $variable is undefined"
        case NoImplicitFound(shape, region) => s"no implicit found for $shape"
        case NoField(t, field, region) => s"$t has no $field field"
        case Message(msg, region) => msg
}