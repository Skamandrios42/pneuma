package pneuma.proto

import general.CompileError
import general.Metadata
import general.Pos

enum TypeError extends CompileError {
    case Mismatch(expected: Program, found: Program, meta: Metadata)
    case Unexpected(expected: String, meta: Metadata)
    case Undefined(variable: String, meta: Metadata)
    case NoImplicitFound(shape: Option[Program], meta: Metadata)
    case NoField(t: Program, field: String, meta: Metadata)
    case Message(msg: String, meta: Metadata)
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