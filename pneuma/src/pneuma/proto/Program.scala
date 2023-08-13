package pneuma.proto

import Program.{ModElem, IntElem, Mode}
import general.*
import generation.Counter

object Program {
    enum Mode { case Exp, Imp }
    case class ModElem(name: String, term: Program, mode: Mode) {
        override def toString(): String = mode match
            case Mode.Exp => s"$name = $term"
            case Mode.Imp => s"?$name = $term"
    }
    case class IntElem(name: String, term: Program, mode: Mode) {
        override def toString(): String = mode match
            case Mode.Exp => s"$name : $term"
            case Mode.Imp => s"?$name : $term"
    }

}

enum Program derives HasMeta {
    case Var(x: String, meta: Metadata = Metadata(None, 0, 0))
    case Abs(x: String, t: Program, meta: Metadata = Metadata(None, 0, 0))
    case App(t1: Program, t2: Program, meta: Metadata = Metadata(None, 0, 0))
    case Typ(meta: Metadata = Metadata(None, 0, 0))
    case Phi(meta: Metadata = Metadata(None, 0, 0))
    case Pro(x: Option[String], t1: Program, t2: Program, meta: Metadata = Metadata(None, 0, 0))
    case Imp(t1: Program, t2: Program, meta: Metadata = Metadata(None, 0, 0))
    case Inf(x: String, t1: Program, t2: Program, meta: Metadata = Metadata(None, 0, 0))
    case Module(fields: List[ModElem], meta: Metadata = Metadata(None, 0, 0))
    case Interface(fields: List[IntElem], meta: Metadata = Metadata(None, 0, 0))
    case Get(t: Program, field: String, meta: Metadata = Metadata(None, 0, 0))
    case As(te: Program, ty: Program, meta: Metadata = Metadata(None, 0, 0))
    case NatType(meta: Metadata = Metadata(None, 0, 0))
    case Nat(value: Int, meta: Metadata = Metadata(None, 0, 0))
    case Succ(t: Program, meta: Metadata = Metadata(None, 0, 0))
    case Debug(t: Program, meta: Metadata = Metadata(None, 0, 0))
    case Match(t: Program, onZero: Program, onSucc: Abs, meta: Metadata = Metadata(None, 0, 0))

    extension (self: Map[String, Term]) def >>(amount: Int) = self.map((s, i) => (s, i >> amount))

    def convert(ctx: Map[String, Term]): Result[TypeError, Term] = this match
        case Program.Var(x, r) if ctx.contains(x) => Result.succeed(ctx(x).metaOf(Term.Var(0, None, r)))
        case Program.Var(x, r) => Result.fail(TypeError.Undefined(x, r))
        case Program.Abs(x, t, r) => t.convert((ctx >> 1) + (x -> Term.Var(0, None, r))).map(Term.Abs(_, r, x))
        case Program.App(t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert(ctx).map(b => Term.App(a, b, r)))
        case Program.Typ(r) => Result.succeed(Term.Typ(r))
        case Program.Phi(r) => Result.succeed(Term.Phi(r))
        case Program.Pro(Some(x), t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert((ctx >> 1) + (x -> Term.Var(0, None, r))).map(b => Term.Pro(a, b, r, x)))
        case Program.Pro(None, t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Pro(a, b, r, "")))
        case Program.Imp(t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert(ctx >> 1).map(b => Term.Imp(a, b, r)))
        case Program.Inf(x, t1, t2, r) => t1.convert(ctx).flatMap(a => t2.convert((ctx >> 1) + (x -> Term.Var(0, None, r))).map(b => Term.Inf(a, b, r, x)))
        case Program.Module(fields, r) => fields.map {
            case ModElem(name, term, Mode.Exp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None, r), mod.name, r)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None, r)) ++ names).map(Term.ModElem(name, _, Term.Mode.Exp))
            case ModElem(name, term, Mode.Imp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None, r), mod.name, r)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None, r)) ++ names).map(Term.ModElem(name, _, Term.Mode.Imp))
        }.acc.map(xs => Term.Module(xs, r))
        case Program.Interface(fields, r) => fields.map {
            case IntElem(name, term, Mode.Exp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None, r), mod.name, r)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None, r)) ++ names).map(Term.IntElem(name, _, Term.Mode.Exp))
            case IntElem(name, term, Mode.Imp) => 
                val names = fields.map(mod => mod.name -> Term.Get(Term.Var(0, None, r), mod.name, r)).toMap
                term.convert((ctx >> 1) + ("this" -> Term.Var(0, None, r)) ++ names).map(Term.IntElem(name, _, Term.Mode.Imp))
        }.acc.map(xs => Term.Interface(xs, r))
        case Program.Get(t, field, r) => t.convert(ctx).map(Term.Get(_, field, r))
        case Program.As(te, ty, r) => te.convert(ctx).flatMap(a => ty.convert(ctx).map(b => Term.As(a, b, r)))
        case Program.NatType(r) => Result.succeed(Term.NatType(r))
        case Program.Nat(value, r) => Result.succeed(Term.Nat(value, r))
        case Program.Succ(t, r) => t.convert(ctx).map(Term.Succ(_, r))
        case Program.Debug(t, r) => t.convert(ctx).map(Term.Debug(_, r))
        case Program.Match(t, onZero, Program.Abs(x, onSucc, _), r) => 
            t.convert(ctx).flatMap(a => onZero.convert(ctx).flatMap(b => onSucc.convert((ctx >> 1) + (x -> Term.Var(0, None, r))).map(c => Term.Match(a, b, c, r, x))))

    override def toString: String = this match
        case Var(x, r) => x
        case Abs(x, t, r) => s"(\\$x -> $t)"
        case App(t1, t2, r) => s"($t1 $t2)"
        case Typ(r) => "Type"
        case Phi(r) => "?"
        case Pro(Some(x), t1, t2, r) => s"(($x : $t1) => $t2)"
        case Pro(None, t1, t2, r) => s"($t1 => $t2)"
        case Imp(t1, t2, r) => s"($t1 =?> $t2)"
        case Inf(x, t1, t2, r) => s"([$x : $t1] => $t2)"
        case Module(fields, r) => s"{ ${fields.mkString(", ")} }"
        case Interface(fields, r) => s"{ ${fields.mkString(", ")} }"
        case Get(t, field, r) => s"($t.$field)"
        case As(te, ty, r) => s"($te : $ty)"
        case NatType(r) => "Nat"
        case Nat(value, r) => s"$value"
        case Succ(t, r) => s"(S $t)"
        case Debug(t, r) => s"(debug $t)"
        case Match(t, onZero, onSucc, r) => s"($t match { $onZero, $onSucc })"
    
}