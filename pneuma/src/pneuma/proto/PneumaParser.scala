package pneuma.proto

import scala.language.implicitConversions
import parsing.Parser
import general.Result
import java.nio.file.{Files, Path}
import scala.util.matching.Regex
import general.Metadata
import scala.collection.immutable.ArraySeq
import parsing.Parser.GetPos

object PneumaParser {

    given GetPos[Source] with {
      extension (self: Source) override def getIndex: Int = self.index
    }

    def str(s: String): Parser[Source, ParseError, String] = source => source.parseStr(s, None) match
        case Result.Success(source, region, s) => (Result.Success(s), source)
        case Result.Failure(values) => (Result.Failure(values), source)

    def regex(r: Regex): Parser[Source, ParseError, String] = source => source.parseRegex(r, None) match
        case Result.Success(source, region, s) => (Result.Success(s), source)
        case Result.Failure(values) => (Result.Failure(values), source)

    given Conversion[String, Parser[Source, ParseError, String]] = str(_)

    given Conversion[Regex, Parser[Source, ParseError, String]] = regex(_)

    def skip = regex("\\s*".r)

    extension [E, A](self: Parser[Source, E, A]) def spaced = skip *> self <* skip

    lazy val ident = "[_a-zA-Z][_a-zA-Z0-9]*".r.transform(identity, _.copy(expected = "identifier"))
    lazy val number = "-?[0-9]+(\\.[0-9]+)?".r.transform(_.toDouble, _.copy(expected = "number"))
    lazy val integer = "-?[0-9]+".r.transform(_.toInt, _.copy(expected = "number"))
    lazy val string = regex("\"([^\"\\\\]|\\\\[\\s\\S])*\"".r).transform(
        str => str.substring(1, str.length - 1),
        err => err.copy(expected = "string-literal")
    )

    lazy val variable = ident.map(Program.Var(_)).track or ("(" *> imp.spaced <* ")")

    lazy val natural = integer.filter(_ >= 0)(
        e => ParseError("non-negative number", "negative number", Metadata(None, e.index, e.index))
    ).map(Program.Nat(_))

    lazy val abstraction: Parser[Source, ParseError, Program.Abs] = for
        _    <- str("\\")
        x    <- ident.spaced
        _    <- str("->").spaced
        body <- imp
    yield Program.Abs(x, body)

    type PParser = Parser[Source, ParseError, Program]

    lazy val modDef = (       ident <*> "=".spaced <*> imp).map { case ((id, _), te) => Program.ModElem(id, te, Program.Mode.Exp) }
    lazy val modImp = ("?" *> ident <*> "=".spaced <*> imp).map { case ((id, _), te) => Program.ModElem(id, te, Program.Mode.Imp) }
    lazy val intDef = (       ident <*> ":".spaced <*> imp).map { case ((id, _), te) => Program.IntElem(id, te, Program.Mode.Exp) }
    lazy val intImp = ("?" *> ident <*> ":".spaced <*> imp).map { case ((id, _), te) => Program.IntElem(id, te, Program.Mode.Imp) }
    lazy val typedDef = (       ident <*> ":".spaced <*> imp <*> "=".spaced <*> imp).map { case ((((id, _), ty), _), te) => (Program.ModElem(id, te, Program.Mode.Exp), Program.IntElem(id, ty, Program.Mode.Exp)) }
    lazy val typedImp = ("?" *> ident <*> ":".spaced <*> imp <*> "=".spaced <*> imp).map { case ((((id, _), ty), _), te) => (Program.ModElem(id, te, Program.Mode.Imp), Program.IntElem(id, ty, Program.Mode.Imp)) }

    lazy val module = "{" *> (modDef or modImp).repeatsep(",".spaced).map(Program.Module(_)).spaced <* "}"
    lazy val interface = "{" *> (intDef or intImp).repeatsep(",".spaced).map(Program.Interface(_)).spaced <* "}"

    lazy val typedModule = "{" *> (typedDef or typedImp).repeatsep(",".spaced).map(xs => Program.As(Program.Module(xs.map(_(0))), Program.Interface(xs.map(_(1))))).spaced <* "}"

    lazy val query = "?".map(_ => Program.Phi())
    lazy val typ = "Type".map(_ => Program.Typ())
    lazy val natType = "Nat".map(_ => Program.NatType())

    lazy val product = for
        _   <- str("(")
        x   <- ident.spaced
        _   <- str(":").spaced
        in  <- imp.spaced
        _   <- str(")").spaced
        _   <- str("=>").spaced
        out <- imp
    yield Program.Pro(Some(x), in, out)

    lazy val inferred = for
        _   <- str("[")
        x   <- ident.spaced
        _   <- str(":").spaced
        in  <- imp.spaced
        _   <- str("]").spaced
        _   <- str("=>").spaced
        out <- imp
    yield Program.Inf(x, in, out)

    lazy val expr = (query or typ or natType or natural or product or inferred or abstraction or variable or typedModule or module or interface).track

    lazy val get: PParser = (expr.track <*> (".".spaced *> ident.tracked(None)).repeat).map {
        case (te, seq) => seq.foldLeft(te) { case (t, (f, r)) => Program.Get(t, f, t.meta join r) }
    }

    lazy val matchStatement = (get <*> ("match".spaced <*> "{" <*> imp.spaced <*> "," <*> abstraction.track.spaced <*> "}").opt).map {
        case (te, Some(((((_, _), onZero), _), onSucc), _)) => Program.Match(te, onZero, onSucc.asInstanceOf[Program.Abs])
        case (te, None) => te
    }.track

    lazy val app: PParser = (matchStatement.track <*> (skip *> matchStatement.tracked(None)).repeat).map {
        case (te, seq) => seq.foldLeft(te) { 
            case (Program.Var("S", r), (arg, r1)) => Program.Succ(arg).attach(r join r1)
            case (Program.Var("debug", r), (arg, r1)) => Program.Debug(arg).attach(r join r1)
            case (abs, (arg, r1)) => Program.App(abs, arg, abs.meta join r1) }
    }

    lazy val app2: PParser = (matchStatement.track <* skip).fold { // here is the error
        case (Program.Var("S", r), arg) => Program.Succ(arg).attach(r join arg.meta)
        case (Program.Var("debug", r), arg) => Program.Debug(arg).attach(r join arg.meta)
        case (abs, arg) => Program.App(abs, arg).attach(abs.meta join arg.meta)
    }

    lazy val as: PParser = (app <*> (str(":").spaced *> as).opt).map {
        case (a, Some(b)) => Program.As(a, b)
        case (a, None) => a
    }.track

    lazy val imp: PParser = (as <*> (("=?>" or "=>").spaced <*> imp).opt).map {
        case (a, Some("=>", b)) => Program.Pro(None, a, b)
        case (a, Some("=?>", b)) => Program.Imp(a, b)
        case (a, None) => a
        case (a, Some(_, b)) => Program.Imp(a, b) // cant happen, will be obsolete with better typing
    }.track

    def removeComments(input: String): String = 
        if input == "" then "" else
            input.takeWhile(_ != '#') ++ removeComments(input.dropWhile(_ != '#').dropWhile(_ != '\n'))

    def apply(input: String): Result[ParseError, Program] = imp.spaced(Source(input, 0))(0)
    def fromFile(name: String) = apply("{" ++ Files.readString(Path.of(name)) ++ "}.main")
}