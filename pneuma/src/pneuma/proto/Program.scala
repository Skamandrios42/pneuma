package pneuma.proto

import Program.{ModElem, IntElem, Mode}
import general.*

object Program {
    enum Mode { case Exp, Imp }
    case class ModElem(name: String, term: Program, mode: Mode)
    case class IntElem(name: String, term: Program, mode: Mode)
}

trait HasRegion {
    def r: Region
}

enum Program extends HasRegion {
    case Var(x: String, r: Region = Region(None, 0, 0))
    case Abs(x: String, t: Program, r: Region = Region(None, 0, 0))
    case App(t1: Program, t2: Program, r: Region = Region(None, 0, 0))
    case Typ(r: Region = Region(None, 0, 0))
    case Phi(r: Region = Region(None, 0, 0))
    case Pro(x: Option[String], t1: Program, t2: Program, r: Region = Region(None, 0, 0))
    case Imp(t1: Program, t2: Program, r: Region = Region(None, 0, 0))
    case Module(fields: List[ModElem], r: Region = Region(None, 0, 0))
    case Interface(fields: List[IntElem], r: Region = Region(None, 0, 0))
    case Get(t: Program, field: String, r: Region = Region(None, 0, 0))
    case As(te: Program, ty: Program, r: Region = Region(None, 0, 0))
    case NatType(r: Region = Region(None, 0, 0))
    case Nat(value: Int, r: Region = Region(None, 0, 0))
    case Succ(t: Program, r: Region = Region(None, 0, 0))
    case Debug(t: Program, r: Region = Region(None, 0, 0))
    case Match(t: Program, onZero: Program, onSucc: Abs, r: Region = Region(None, 0, 0))

    extension (self: Map[String, Term]) def >>(amount: Int) = self.map((s, i) => (s, i >> amount))

    def convert(ctx: Map[String, Term]): Result[TypeError, Term] = this match
        case Program.Var(x, r) if ctx.contains(x) => Result.succeed(ctx(x))
        case Program.Var(x, r) => Result.fail(TypeError.Undefined(x, Region(None, 0, 0)))
        case Program.Abs(x, t, r) => t.convert((ctx >> 1) + (x -> Term.Var(0, None))).map(Term.Abs(_))
        case Program.App(t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert(ctx).map(b => Term.App(a, b)))
        case Program.Typ(r) => Result.succeed(Term.Typ)
        case Program.Phi(r) => Result.succeed(Term.Phi)
        case Program.Pro(Some(x), t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert((ctx >> 1) + (x -> Term.Var(0, None))).map(b => Term.Pro(a, b)))
        case Program.Pro(None, t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Pro(a, b)))
        case Program.Imp(t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Imp(a, b)))
        case Program.Module(fields, r) => fields.map {
            case ModElem(name, term, Mode.Exp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.ModElem(name, _, Term.Mode.Exp))
            case ModElem(name, term, Mode.Imp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.ModElem(name, _, Term.Mode.Imp))
        }.acc.map(xs => Term.Module(xs))
        case Program.Interface(fields, r) => fields.map {
            case IntElem(name, term, Mode.Exp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.IntElem(name, _, Term.Mode.Exp))
            case IntElem(name, term, Mode.Imp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None), mod.name)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None)) ++ names).map(Term.IntElem(name, _, Term.Mode.Imp))
        }.acc.map(xs => Term.Interface(xs))
        case Program.Get(t, field, r) => t.convert(ctx).map(Term.Get(_, field))
        case Program.As(te, ty, r) => te.convert(ctx).flatMap(a => ty.convert(ctx).map(b => Term.As(a, b)))
        case Program.NatType(r) => Result.succeed(Term.NatType)
        case Program.Nat(value, r) => Result.succeed(Term.Nat(value))
        case Program.Succ(t, r) => t.convert(ctx).map(Term.Succ(_))
        case Program.Debug(t, r) => t.convert(ctx).map(Term.Debug(_))
        case Program.Match(t, onZero, Program.Abs(x, onSucc, _), r) => 
            t.convert(ctx).flatMap(a => onZero.convert(ctx).flatMap(b => onSucc.convert((ctx >> 1) + (x -> Term.Var(0, None))).map(c => Term.Match(a, b, c))))

}