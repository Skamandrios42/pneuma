package pneuma.proto

import scala.language.implicitConversions
import parsing.Parser
import parsing.Parser.Result
import parsing.StringParsers.{*, given}

object PneumaParser {

    enum ModElem {
        case Named(name: String, term: Program)
        case Imp(name: String, term: Program)
    }

    enum IntElem {
        case Named(name: String, typ: Program)
        case Imp(name: String, typ: Program)
    }

    enum Program {
        case Var(x: String) // DONE
        case Abs(x: String, t: Program) // DONE
        case App(t1: Program, t2: Program) // DONE
        case Typ // DONE
        case Phi // DONE
        case Pro(x: Option[String], t1: Program, t2: Program) // DONE
        case Imp(t1: Program, t2: Program) // DONE
        case Module(fields: List[ModElem]) // DONE
        case Interface(fields: List[IntElem]) // DONE
        case Get(t: Program, field: String) // DONE
        case As(te: Program, ty: Program) // DONE

        case NatType // DONE
        case Nat(value: Int) // DONE
        case Succ(t: Program) // DONE
        case Match(t: Program, onZero: Program, onSucc: Abs)
    }

    extension (self: Map[String, Int]) def >>(amount: Int) = self.map((s, i) => (s, i + amount))

    def convert(program: Program, ctx: Map[String, Int]): Either[String, Term] = program match
        case Program.Var(x) if ctx.contains(x) => Right(Term.Var(ctx(x), None))
        case Program.Var(x) => Left(s"undefined variable $x")
        case Program.Abs(x, t) => convert(t, (ctx >> 1) + (x -> 0)).map(Term.Abs(_))
        case Program.App(t1, t2) => convert(t1, ctx).flatMap(a => convert(t2, ctx).map(b => Term.App(a, b)))
        case Program.Typ => Right(Term.Typ)
        case Program.Phi => Right(Term.Phi)
        case Program.Pro(Some(x), t1, t2) => convert(t1, ctx).flatMap(a => convert(t2, (ctx >> 1) + (x -> 0)).map(b => Term.Pro(a, b)))
        case Program.Pro(None, t1, t2) => convert(t1, ctx).flatMap(a => convert(t2, ctx >> 1).map(b => Term.Pro(a, b)))
        case Program.Imp(t1, t2) => convert(t1, ctx).flatMap(a => convert(t2, ctx >> 1).map(b => Term.Imp(a, b)))
        case Program.Module(fields) => fields.foldLeft[Either[String, List[pneuma.proto.ModElem]]](Right(Nil)) { 
            case (Right(xs), ModElem.Named(name, term)) => convert(term, (ctx >> 1) + ("this" -> 0)).map(pneuma.proto.ModElem(name, _, Mode.Exp) :: xs)
            case (Right(xs), ModElem.Imp(name, term))   => convert(term, (ctx >> 1) + ("this" -> 0)).map(pneuma.proto.ModElem(name, _, Mode.Imp) :: xs)
            case (Left(msg), _) => Left(msg)
        }.map(Term.Module(_))
        case Program.Interface(fields) => fields.foldLeft[Either[String, List[pneuma.proto.IntElem]]](Right(Nil)) { 
            case (Right(xs), IntElem.Named(name, typ)) => convert(typ, (ctx >> 1) + ("this" -> 0)).map(pneuma.proto.IntElem(name, _, Mode.Exp) :: xs)
            case (Right(xs), IntElem.Imp(name, typ))   => convert(typ, (ctx >> 1) + ("this" -> 0)).map(pneuma.proto.IntElem(name, _, Mode.Imp) :: xs)
            case (Left(msg), _) => Left(msg)
        }.map(Term.Interface(_))
        case Program.Get(t, field) => convert(t, ctx).map(Term.Get(_, field))
        case Program.As(te, ty) => convert(te, ctx).flatMap(a => convert(ty, ctx).map(b => Term.As(a, b)))
        case Program.NatType => Right(Term.NatType)
        case Program.Nat(value) => Right(Term.Nat(value))
        case Program.Succ(t) => convert(t, ctx).map(Term.Succ(_))
        case Program.Match(t, onZero, Program.Abs(x, onSucc)) => 
            convert(t, ctx).flatMap(a => convert(onZero, ctx).flatMap(b => convert(onSucc, (ctx >> 1) + (x -> 0)).map(c => Term.Match(a, b, c))))

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

    lazy val modDef = (       ident <*> "=".spaced <*> imp).map { case ((id, _), te) => ModElem.Named(id, te) }
    lazy val modImp = ("?" *> ident <*> "=".spaced <*> imp).map { case ((id, _), te) => ModElem.Imp(id, te) }
    lazy val intDef = (       ident <*> ":".spaced <*> imp).map { case ((id, _), te) => IntElem.Named(id, te) }
    lazy val intImp = ("?" *> ident <*> ":".spaced <*> imp).map { case ((id, _), te) => IntElem.Imp(id, te) }
    
    lazy val module = "{" *> (modDef or modImp).repeatsep(",".spaced).map(Program.Module(_)).spaced <* "}"
    lazy val interface = "{" *> (intDef or intImp).repeatsep(",".spaced).map(Program.Interface(_)).spaced <* "}"
    
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

    lazy val expr = query or typ or natType or natural or product or abstraction or variable or module or interface

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
}