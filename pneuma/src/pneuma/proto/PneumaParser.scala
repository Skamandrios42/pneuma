package pneuma.proto

import scala.language.implicitConversions
import parsing.Parser
import parsing.Parser.Result
import parsing.StringParsers.{*, given}
import java.nio.file.{Files, Path}

object PneumaParser {

    lazy val ident = regex("[_a-zA-Z][_a-zA-Z0-9]*".r).transform(identity, _.copy(expected = "identifier"))
    lazy val number = "-?[0-9]+(\\.[0-9]+)?".r.transform(_.toDouble, _.copy(expected = "number"))
    lazy val integer = "-?[0-9]+".r.transform(_.toInt, _.copy(expected = "number"))
    lazy val string = regex("\"([^\"\\\\]|\\\\[\\s\\S])*\"".r).transform(
        str => str.substring(1, str.length - 1), 
        err => err.copy(expected = "string-literal")
    )

    lazy val variable = ident.map(Program.Var(_)) or ("(" *> imp.spaced <* ")")

    lazy val natural = integer.filter(_ >= 0)((_, p) => StringError("non-negative number", "negative number", p)).map(Program.Nat(_))

    lazy val abstraction = for 
        _    <- str("\\")
        x    <- ident.spaced
        _    <- str("->").spaced
        body <- imp
    yield Program.Abs(x, body) : Program.Abs

    type PParser = Parser[(String, Int), StringError, Program]

    lazy val modDef = (       ident <*> "=".spaced <*> imp).map { case ((id, _), te) => Program.ModElem(id, te, Program.Mode.Exp) }
    lazy val modImp = ("?" *> ident <*> "=".spaced <*> imp).map { case ((id, _), te) => Program.ModElem(id, te, Program.Mode.Imp) }
    lazy val intDef = (       ident <*> ":".spaced <*> imp).map { case ((id, _), te) => Program.IntElem(id, te, Program.Mode.Exp) }
    lazy val intImp = ("?" *> ident <*> ":".spaced <*> imp).map { case ((id, _), te) => Program.IntElem(id, te, Program.Mode.Imp) }
    lazy val typedDef = (       ident <*> ":".spaced <*> imp <*> "=".spaced <*> imp).map { case ((((id, _), ty), _), te) => (Program.ModElem(id, te, Program.Mode.Exp), Program.IntElem(id, ty, Program.Mode.Exp)) }
    lazy val typedImp = ("?" *> ident <*> ":".spaced <*> imp <*> "=".spaced <*> imp).map { case ((((id, _), ty), _), te) => (Program.ModElem(id, te, Program.Mode.Imp), Program.IntElem(id, ty, Program.Mode.Imp)) }
    
    lazy val module = "{" *> (modDef or modImp).repeatsep(",".spaced).map(Program.Module(_)).spaced <* "}"
    lazy val interface = "{" *> (intDef or intImp).repeatsep(",".spaced).map(Program.Interface(_)).spaced <* "}"
    
    lazy val typedModule = "{" *> (typedDef or typedImp).repeatsep(",".spaced).map(xs => Program.As(Program.Module(xs.map(_(0))), Program.Interface(xs.map(_(1))))).spaced <* "}"

    lazy val query = "?".map(_ => Program.Phi)
    lazy val typ = "Type".map(_ => Program.Typ)
    lazy val natType = "Nat".map(_ => Program.NatType)
    
    lazy val product = for 
        _   <- str("(")
        x   <- ident.spaced
        _   <- str(":").spaced
        in  <- imp.spaced
        _   <- str(")").spaced
        _   <- str("=>").spaced
        out <- imp
    yield Program.Pro(Some(x), in, out)

    lazy val expr = query or typ or natType or natural or product or abstraction or variable or typedModule or module or interface

    lazy val get: PParser = (expr <*> (".".spaced *> ident).repeat).map { 
        case (te, seq) => seq.foldLeft(te)((t, f) => Program.Get(t, f)) 
    }

    lazy val matchStatement = (get <*> ("match".spaced <*> "{" <*> imp.spaced <*> "," <*> abstraction.spaced <*> "}").opt).map {
        case (te, Some(((((_, _), onZero), _), onSucc), _)) => Program.Match(te, onZero, onSucc)
        case (te, None) => te
    }

    lazy val app: PParser = (matchStatement <* skip).fold {
        case (Program.Var("S"), arg) => Program.Succ(arg)
        case (abs, arg) => Program.App(abs, arg)
    }

    lazy val as: PParser = (app <*> (str(":").spaced *> as).opt).map {
        case (a, Some(b)) => Program.As(a, b)
        case (a, None) => a
    }

    lazy val imp: PParser = (as <*> (("=?>" or "=>").spaced <*> imp).opt).map {
        case (a, Some("=>", b)) => Program.Pro(None, a, b)
        case (a, Some("=?>", b)) => Program.Imp(a, b)
        case (a, None) => a
        case (a, Some(_, b)) => Program.Imp(a, b) // cant happen, will be obsolete with better typing
    }

    def apply(input: String): Result[StringError, Program] = imp.spaced(input, 0)._1
    def fromFile(name: String) = apply("{" ++ Files.readString(Path.of(name)) ++ "}")
}