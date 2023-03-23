package pneuma.proto

import Program.{ModElem, IntElem, Mode}
import general.*

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

    extension (self: Map[String, Term]) def >>(amount: Int) = self.map((s, i) => (s, i >> amount))

    def convert(ctx: Map[String, Term]): Result[TypeError, Term] = this match
        case Program.Var(x) if ctx.contains(x) => Result.succeed(ctx(x))
        case Program.Var(x) => Result.fail(TypeError.Undefined(x, Region(None, Pos(0,0), Pos(0,0))))
        case Program.Abs(x, t) => t.convert((ctx >> 1) + (x -> Term.Var(0, None))).map(Term.Abs(_))
        case Program.App(t1, t2) => t1.convert(ctx).flatMap(a => t2.convert(ctx).map(b => Term.App(a, b)))
        case Program.Typ => Result.succeed(Term.Typ)
        case Program.Phi => Result.succeed(Term.Phi)
        case Program.Pro(Some(x), t1, t2) => t1.convert(ctx).flatMap(a => t2.convert((ctx >> 1) + (x -> Term.Var(0, None))).map(b => Term.Pro(a, b)))
        case Program.Pro(None, t1, t2) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Pro(a, b)))
        case Program.Imp(t1, t2) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Imp(a, b)))
        case Program.Module(fields) => fields.map {
            case ModElem(name, term, Mode.Exp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.ModElem(name, _, Term.Mode.Exp))
            case ModElem(name, term, Mode.Imp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.ModElem(name, _, Term.Mode.Imp))
        }.acc.map(xs => Term.Module(xs))
        case Program.Interface(fields) => fields.map {
            case IntElem(name, term, Mode.Exp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.IntElem(name, _, Term.Mode.Exp))
            case IntElem(name, term, Mode.Imp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.IntElem(name, _, Term.Mode.Imp))
        }.acc.map(xs => Term.Interface(xs))
        case Program.Get(t, field) => t.convert(ctx).map(Term.Get(_, field))
        case Program.As(te, ty) => te.convert(ctx).flatMap(a => ty.convert(ctx).map(b => Term.As(a, b)))
        case Program.NatType => Result.succeed(Term.NatType)
        case Program.Nat(value) => Result.succeed(Term.Nat(value))
        case Program.Succ(t) => t.convert(ctx).map(Term.Succ(_))
        case Program.Debug(t) => t.convert(ctx).map(Term.Debug(_))
        case Program.Match(t, onZero, Program.Abs(x, onSucc)) => 
            t.convert(ctx).flatMap(a => onZero.convert(ctx).flatMap(b => onSucc.convert((ctx >> 1) + (x -> Term.Var(0, None))).map(c => Term.Match(a, b, c))))

}