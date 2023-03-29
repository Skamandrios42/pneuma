package pneuma.proto

import general.CompileError
import general.Region
import general.Pos

enum TypeError extends CompileError {
    case Mismatch(expected: Program, found: Program, region: Region)
    case Unexpected(expected: String, region: Region)
    case Undefined(variable: String, region: Region)
    case NoImplicitFound(shape: Option[Program], region: Region)
    case NoField(t: Program, field: String, region: Region)
    case Message(msg: String, region: Region)
    def name = "Type Error"
    def message: String = this match
        case Mismatch(expected, found, region) => s"expected '$expected', but found '$found'"
        case Unexpected(expected, region) => s"expected $expected"
        case Undefined(variable, region) => s"variable $variable is undefined"
        case NoImplicitFound(Some(shape), region) => s"no implicit found for $shape"
        case NoImplicitFound(None, region) => s"no implicit for unknown type"
        case NoField(t, field, region) => s"$t has no $field field"
        case Message(msg, region) => msg
}