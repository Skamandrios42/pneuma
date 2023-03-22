package pneuma.proto

import Program.{ModElem, IntElem, Mode}

object Program {
    enum Mode { case Exp, Imp }
    case class ModElem(name: String, term: Program, mode: Mode)
    case class IntElem(name: String, term: Program, mode: Mode)
}

enum Program {
    case Var(x: String)
    case Abs(x: String, t: Program)
    case App(t1: Program, t2: Program)
    case Typ
    case Phi
    case Pro(x: Option[String], t1: Program, t2: Program)
    case Imp(t1: Program, t2: Program)
    case Module(fields: List[ModElem])
    case Interface(fields: List[IntElem])
    case Get(t: Program, field: String)
    case As(te: Program, ty: Program)
    case NatType
    case Nat(value: Int)
    case Succ(t: Program)
    case Debug(t: Program)
    case Match(t: Program, onZero: Program, onSucc: Abs)

    extension (self: Map[String, Int]) def >>(amount: Int) = self.map((s, i) => (s, i + amount))

    def convert(ctx: Map[String, Int]): Either[String, Term] = this match
        case Program.Var(x) if ctx.contains(x) => Right(Term.Var(ctx(x), None))
        case Program.Var(x) => Left(s"undefined variable $x")
        case Program.Abs(x, t) => t.convert((ctx >> 1) + (x -> 0)).map(Term.Abs(_))
        case Program.App(t1, t2) => t1.convert(ctx).flatMap(a => t2.convert(ctx).map(b => Term.App(a, b)))
        case Program.Typ => Right(Term.Typ)
        case Program.Phi => Right(Term.Phi)
        case Program.Pro(Some(x), t1, t2) => t1.convert(ctx).flatMap(a => t2.convert((ctx >> 1) + (x -> 0)).map(b => Term.Pro(a, b)))
        case Program.Pro(None, t1, t2) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Pro(a, b)))
        case Program.Imp(t1, t2) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Imp(a, b)))
        case Program.Module(fields) => fields.foldLeft[Either[String, List[Term.ModElem]]](Right(Nil)) { 
            case (Right(xs), ModElem(name, term, Mode.Exp)) => term.convert((ctx >> 1) + ("this" -> 0)).map(Term.ModElem(name, _, Term.Mode.Exp) :: xs)
            case (Right(xs), ModElem(name, term, Mode.Imp)) => term.convert((ctx >> 1) + ("this" -> 0)).map(Term.ModElem(name, _, Term.Mode.Imp) :: xs)
            case (Left(msg), _) => Left(msg)
        }.map(xs => Term.Module(xs.reverse))
        case Program.Interface(fields) => fields.foldLeft[Either[String, List[Term.IntElem]]](Right(Nil)) { 
            case (Right(xs), IntElem(name, typ, Mode.Exp)) => typ.convert((ctx >> 1) + ("this" -> 0)).map(Term.IntElem(name, _, Term.Mode.Exp) :: xs)
            case (Right(xs), IntElem(name, typ, Mode.Imp)) => typ.convert((ctx >> 1) + ("this" -> 0)).map(Term.IntElem(name, _, Term.Mode.Imp) :: xs)
            case (Left(msg), _) => Left(msg)
        }.map(xs => Term.Interface(xs.reverse))
        case Program.Get(t, field) => t.convert(ctx).map(Term.Get(_, field))
        case Program.As(te, ty) => te.convert(ctx).flatMap(a => ty.convert(ctx).map(b => Term.As(a, b)))
        case Program.NatType => Right(Term.NatType)
        case Program.Nat(value) => Right(Term.Nat(value))
        case Program.Succ(t) => t.convert(ctx).map(Term.Succ(_))
        case Program.Debug(t) => t.convert(ctx).map(Term.Debug(_))
        case Program.Match(t, onZero, Program.Abs(x, onSucc)) => 
            t.convert(ctx).flatMap(a => onZero.convert(ctx).flatMap(b => onSucc.convert((ctx >> 1) + (x -> 0)).map(c => Term.Match(a, b, c))))

}