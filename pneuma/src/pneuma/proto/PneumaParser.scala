package pneuma.proto

import scala.language.implicitConversions
    import parsing.Parser
    import parsing.Parser.Result
    import parsing.StringParsers.{*, given}

object PneumaParser {

    enum ModElem {
        case Named(name: String, term: Program)
        case Imp(typ: Program, term: Program)
    }

    enum IntElem {
        case Named(name: String, typ: Program)
        case Imp(typ: Program)
    }

    enum Program {
        case Var(x: String) // DONE
        case Abs(x: String, t: Program) // DONE
        case App(t1: Program, t2: Program) // DONE
        case Typ // DONE
        case Phi // DONE
        case Pro(x: String, t1: Program, t2: Program) // DONE
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

    def convert(program: Program, ctx: Map[String, Int]): Term = program match
        case Program.Var(x) => Term.Var(ctx(x), None)
        case Program.Abs(x, t) => Term.Abs(convert(t, (ctx >> 1) + (x -> 0)))
        case Program.App(t1, t2) => Term.App(convert(t1, ctx), convert(t2, ctx))
        case Program.Typ => Term.Typ
        case Program.Phi => Term.Phi
        case Program.Pro(x, t1, t2) => Term.Pro(convert(t1, ctx), convert(t2, (ctx >> 1) + (x -> 0)))
        case Program.Imp(t1, t2) => Term.Imp(convert(t1, ctx), convert(t2, ctx >> 1))
        case Program.Module(fields) => Term.Module(fields.map {
            case ModElem.Named(name, term) => pneuma.proto.ModElem.Named(name, convert(term, (ctx >> 1) + ("this" -> 0)))
            case ModElem.Imp(typ, term) => pneuma.proto.ModElem.Imp("", convert(typ, ctx), convert(term,  (ctx >> 1) + ("this" -> 0)))
        })
        case Program.Interface(fields) => Term.Interface(fields.map {
            case IntElem.Named(name, term) => pneuma.proto.IntElem.Named(name, convert(term,  (ctx >> 1) + ("this" -> 0)))
            case IntElem.Imp(typ) => pneuma.proto.IntElem.Imp("", convert(typ,  (ctx >> 1) + ("this" -> 0)))
        })
        case Program.Get(t, field) => Term.Get(convert(t, ctx), field)
        case Program.As(te, ty) => Term.As(convert(te, ctx), convert(ty, ctx))
        case Program.NatType => Term.NatType
        case Program.Nat(value) => Term.Nat(value)
        case Program.Succ(t) => Term.Succ(convert(t, ctx))
        case Program.Match(t, onZero, onSucc) => Term.Match(convert(t, ctx), convert(onZero, ctx), convert(onSucc, ctx))

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

    lazy val modDef = (ident <*> "=".spaced <*> imp).map { case ((id, _), te) => ModElem.Named(id, te) }
    lazy val modImp = ("?" *> imp.spaced <*> "=".spaced <*> imp).map { case ((id, _), te) => ModElem.Imp(id, te) }
    lazy val intDef = (ident <*> ":".spaced <*> imp).map { case ((id, _), te) => IntElem.Named(id, te) }
    lazy val intImp = ("?".spaced *> imp).map(IntElem.Imp(_))
    
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
    yield Program.Pro(x, in, out)

    lazy val expr = query or typ or natType or natural or product or abstraction or variable or module or interface

    lazy val get: PParser = (expr <*> (".".spaced *> ident).repeat).map { 
        case (te, seq) => seq.foldLeft(te)((t, f) => Program.Get(t, f)) 
    }

    lazy val matchStatement = (get <*> ("match".spaced <*> "{" <*> imp <*> "," <*> abstraction <*> "}").opt).map {
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

    lazy val imp: PParser = (as <*> (str("=?>").spaced *> imp).opt).map {
        case (a, Some(b)) => Program.Imp(a, b)
        case (a, None) => a
    }

    def apply(input: String): Result[StringError, Program] = imp.spaced(input, 0)._1
}