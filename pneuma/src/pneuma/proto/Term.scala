package pneuma.proto

import scala.annotation.targetName
import Term.{IntElem, ModElem, Mode, Req}
import general.{Result, Metadata, HasMeta}

object Term {
    enum Mode { case Exp, Imp }

    enum Req:
        case Inf(t: Term)
        case Imp(t: Term)

    case class ModElem(name: String, term: Term, mode: Mode) {
        def onTerm(f: Term => Term) = ModElem(name, f(term), mode)
        override def toString: String = mode match
            case Mode.Exp => s"$name = $term"
            case Mode.Imp => s"?$name = $term"
    }

    case class IntElem(name: String, term: Term, mode: Mode) {
        def onTerm(f: Term => Term) = IntElem(name, f(term), mode)
        override def toString: String = mode match
            case Mode.Exp => s"$name : $term"
            case Mode.Imp => s"?$name : $term"
    }

    extension [A](self: List[ModElem]) {
        /** maps f over every occurrence of [[Term]]
          * @param f a function from Term to Term
          * @return the mapped ModElem
          */
        @targetName("mapValuesModElem")
        def mapValues(f: Term => Term) = self.map {
            case ModElem(name, term, mode) => ModElem(name, f(term), mode)
        }
    }

    extension [A](self: List[IntElem]) {
        /** maps f over every occurrence of [[Term]]
          * @param f a function from Term to Term
          * @return the mapped IntElem
          */
        @targetName("mapValuesIntElem")
        def mapValues(f: Term => Term) = self.map {
            case IntElem(name, term, mode) => IntElem(name, f(term), mode)
        }
    }

}

/** The abstract syntax tree representation of a pneuma program 
  * Use [[DSL]] to define terms in _Scala_
  */
enum Term derives HasMeta {
    case Var(x: Int, tagged: Option[Term], meta: Metadata)
    case Abs(t: Term, meta: Metadata, x: String)
    case App(t1: Term, t2: Term, meta: Metadata)
    case Typ(meta: Metadata)
    case Phi(meta: Metadata)
    case Pro(t1: Term, t2: Term, meta: Metadata, x: String)
    case Imp(t1: Term, t2: Term, meta: Metadata)
    case Inf(t1: Term, t2: Term, meta: Metadata, x: String)
    case Module(fields: List[ModElem], meta: Metadata)
    case Interface(fields: List[IntElem], meta: Metadata)
    case Get(t: Term, field: String, meta: Metadata)
    case As(te: Term, ty: Term, meta: Metadata)
    case NatType(meta: Metadata)
    case Nat(value: Int, meta: Metadata)
    case Succ(t: Term, meta: Metadata)
    case Debug(t: Term, meta: Metadata)
    case Match(t: Term, onZero: Term, onSucc: Term, meta: Metadata, x: String)

    /** maps indices to types */
    type G = Map[Int, Term]
    /** maps types to variables or module accesses */
    type I = Map[Term, Term]
    /** relation between terms */
    type R = Set[(Term, Term)]

    type C = Map[Int, String]

    extension (self: G) {
        /** shifts all elements in the collection by amount */
        @targetName("shiftG")
        def >>(amount: Int): G = self.map { case (key, value) => (key + amount, value >> amount) }
    }

    extension (self: I) {
        /** shifts all elements in the collection by amount */
        @targetName("shiftI")
        def >>(amount: Int): I = self.map { case (key, value) => (key >> amount, value >> amount) }
        @targetName("tagI")
        def tag(y: Int, t: Term): I = self.map { case (key, value) => (key.tag(y, t), value.tag(y, t)) }
    }

    extension (self: R) {
        @targetName("shiftR")
        /** shifts all elements in the collection by amount */
        def >>(amount: Int): R = self.map { case (key, value) => (key >> amount, value >> amount) }
    }

    extension (self: C) {
        @targetName("shiftC")
        def >>(amount: Int): C = self.map((i, s) => (i + 1, s))
    }

    def put(ctx: C, s: String): (String, C) = 
        if s != "" 
        then if ctx.values.forall(_ != s) then (s, (ctx >> 1) + (0 -> s)) else put(ctx, s"$s'")
        else ("", ctx)

    /** generates a simple string representation of the program */
    override def toString: String = this match
        case Var(x, Some(tag), _) => s"$x"
        case Var(x, None, _) => s"$x"
        case Abs(t, _, x) => s"(\\|$x| -> $t)"
        case App(t1, t2, _) => s"($t1 $t2)"
        case Typ(_) => "Type"
        case Phi(_) => "?"
        case Pro(t1, t2, _, x) => s"($t1 |$x| => $t2)"
        case Inf(t1, t2, _, x) => s"([$t1 |$x|] => $t2)"
        case Imp(t1, t2, _) => s"($t1 =?> $t2)"
        case Module(fields, _) => s"{ ${fields.mkString(", ")} }"
        case Interface(fields, _) => s"{ ${fields.mkString(", ")} }"
        case Get(t, field, _) => s"($t.$field)"
        case As(te, ty, _) => s"($te : $ty)"
        case NatType(_) => "Nat"
        case Nat(value, _) => s"nat($value)"
        case Succ(t, _) => s"(S $t)"
        case Debug(t, _) => s"(debug $t)"
        case Match(t, onZero, onSucc, _, x) => s"($t match { $onZero, \\[$x] -> $onSucc })"

    /** adds `amount` to all variables with distance of `cutoff` or longer
      * @param amount number to be added
      * @param cutoff border between shifted and not shifted variables
      * @return the same term, with shifted variables
      */
    def shift(amount: Int, cutoff: Int): Term = this match
        case Var(x, Some(t), _) => if x >= cutoff then Var(x + amount, Some(t.shift(amount, cutoff)), this.meta) else Var(x, Some(t.shift(amount, cutoff)), this.meta)
        case Var(x, None, _) => if x >= cutoff then Var(x + amount, None, this.meta) else Var(x, None, this.meta)
        case Abs(t, _, x) => Abs(t.shift(amount, cutoff + 1), this.meta, x)
        case App(t1, t2, _) => App(t1.shift(amount, cutoff), t2.shift(amount, cutoff), this.meta)
        case Typ(_) => Typ(this.meta)
        case Phi(_) => Phi(this.meta)
        case Pro(t1, t2, _, x) => Pro(t1.shift(amount, cutoff), t2.shift(amount, cutoff + 1), this.meta, x)
        case Inf(t1, t2, _, x) => Inf(t1.shift(amount, cutoff), t2.shift(amount, cutoff + 1), this.meta, x)
        case Imp(t1, t2, _) => Imp(t1.shift(amount, cutoff), t2.shift(amount, cutoff + 1), this.meta)
        case Module(fields, _) => Module(fields.mapValues(_.shift(amount, cutoff + 1)), this.meta)
        case Interface(fields, _) => Interface(fields.mapValues(_.shift(amount, cutoff + 1)), this.meta)
        case Get(t, field, _) => Get(t.shift(amount, cutoff), field, this.meta)
        case As(te, ty, _) => Term.As(te.shift(amount, cutoff), ty.shift(amount, cutoff), this.meta)
        case NatType(r) => NatType(this.meta)
        case Nat(value, _) => Nat(value, this.meta)
        case Succ(t, _) => Succ(t.shift(amount, cutoff), this.meta)
        case Debug(t, _) => Debug(t.shift(amount, cutoff), this.meta)
        case Match(t, onZero, onSucc, _, x) => Match(t.shift(amount, cutoff), onZero.shift(amount, cutoff), onSucc.shift(amount, cutoff + 1), this.meta, x)

    /** returns [[shift shift(amount, 0)]] */
    def >>(amount: Int) = shift(amount, 0)

    /** returns [[shift shift(-amount, 0)]] */
    def <<(amount: Int) = shift(-amount, 0)

    /** replaces all occurrences of a variable by `t`
      * @param y the variable to be replaces
      * @param t the term to be inserted
      * @return `this` with replaced variables
      */
    def replace(y: Int, t: Term): Term = this match
        case Var(x, Some(e), _) => if x == y then t.metaOf(this) else Var(x, Some(e.replace(y, t)), this.meta)
        case Var(x, None, _) => if x == y then t.metaOf(this) else Var(x, None, this.meta)
        case Abs(t1, _, x) => Abs(t1.replace(y + 1, t >> 1), this.meta, x)
        case App(t1, t2, _) => App(t1.replace(y, t), t2.replace(y, t), this.meta)
        case Typ(_) => Typ(this.meta)
        case Phi(_) => Phi(this.meta)
        case Pro(t1, t2, _, x) => Pro(t1.replace(y, t), t2.replace(y + 1, t >> 1), this.meta, x)
        case Inf(t1, t2, _, x) => Inf(t1.replace(y, t), t2.replace(y + 1, t >> 1), this.meta, x)
        case Imp(t1, t2, _) => Imp(t1.replace(y, t), t2.replace(y + 1, t >> 1), this.meta)
        case Module(fields, _) => Module(fields.mapValues(_.replace(y + 1, t >> 1)), this.meta)
        case Interface(fields, _) => Interface(fields.mapValues(_.replace(y + 1, t >> 1)), this.meta)
        case Get(t1, field, _) => Get(t1.replace(y, t), field, this.meta)
        case As(te, ty, _) => Term.As(te.replace(y, t), ty.replace(y, t), this.meta)
        case NatType(_) => NatType(this.meta)
        case Nat(value, _) => Nat(value, this.meta)
        case Succ(e, _) => Succ(e.replace(y, t), this.meta)
        case Debug(e, _) => Debug(e.replace(y, t), this.meta)
        case Match(e, onZero, onSucc, _, x) => Match(e.replace(y, t), onZero.replace(y, t), onSucc.replace(y + 1, t >> 1), this.meta, x)

    /** similiar to [[replace]], but instead of replacing, `t` is added as in the `tagged` field of the variable
      * @param y the variable to be tagged
      * @param t the term to be inserted
      * @return `this` with tagged variables
      */
    def tag(y: Int, t: Term): Term = this match
        case Var(x, Some(e), _) => if x == y then Var(x, Some(t), this.meta) else Var(x, Some(e.tag(y, t)), this.meta)
        case Var(x, None, _) => if x == y then Var(x, Some(t), this.meta) else Var(x, None, this.meta)
        case Abs(t1, _, x) => Abs(t1.tag(y + 1, t >> 1), this.meta, x)
        case App(t1, t2, _) => App(t1.tag(y, t), t2.tag(y, t), this.meta)
        case Typ(_) => Typ(this.meta)
        case Phi(_) => Phi(this.meta)
        case Pro(t1, t2, _, x) => Pro(t1.tag(y, t), t2.tag(y + 1, t >> 1), this.meta, x)
        case Inf(t1, t2, _, x) => Inf(t1.tag(y, t), t2.tag(y + 1, t >> 1), this.meta, x)
        case Imp(t1, t2, _) => Imp(t1.tag(y, t), t2.tag(y + 1, t >> 1), this.meta)
        case Module(fields, _) => Module(fields.mapValues(_.tag(y + 1, t >> 1)), this.meta)
        case Interface(fields, _) => Interface(fields.mapValues(_.tag(y + 1, t >> 1)), this.meta)
        case Get(t1, field, _) => Get(t1.tag(y, t), field, this.meta)
        case As(te, ty, _) => Term.As(te.tag(y, t), ty.tag(y, t), this.meta)
        case NatType(_) => NatType(this.meta)
        case Nat(value, _) => Nat(value, this.meta)
        case Succ(e, _) => Succ(e.tag(y, t), this.meta)
        case Debug(e, _) => Debug(e.tag(y, t), this.meta)
        case Match(e, onZero, onSucc, _, x) => Match(e.tag(y, t), onZero.tag(y, t), onSucc.tag(y + 1, t >> 1), this.meta, x)

    /** removes all tags from the variables */
    def untag: Term = this match
        case Var(x, _, _) => Var(x, None, this.meta)
        case Abs(t1, _, x) => Abs(t1.untag, this.meta, x)
        case App(t1, t2, _) => App(t1.untag, t2.untag, this.meta)
        case Typ(_) => Typ(this.meta)
        case Phi(_) => Phi(this.meta)
        case Pro(t1, t2, _, x) => Pro(t1.untag, t2.untag, this.meta, x)
        case Inf(t1, t2, _, x) => Pro(t1.untag, t2.untag, this.meta, x)
        case Imp(t1, t2, _) => Imp(t1.untag, t2.untag, this.meta)
        case Module(fields, _) => Module(fields.mapValues(_.untag), this.meta)
        case Interface(fields, _) => Interface(fields.mapValues(_.untag), this.meta)
        case Get(t1, field, _) => Get(t1.untag, field, this.meta)
        case As(te, ty, _) => Term.As(te.untag, ty.untag, this.meta)
        case NatType(_) => NatType(this.meta)
        case Nat(value, _) => Nat(value, this.meta)
        case Succ(e, _) => Succ(e.untag, this.meta)
        case Debug(e, _) => Debug(e.untag, this.meta)
        case Match(t, onZero, onSucc, _, x) => Match(t.untag, onZero.untag, onSucc.untag, this.meta, x)

    def erase: Term = this match
        case Var(x, tagged, _) => Var(x, tagged.map(_.erase), Metadata.none)
        case Abs(t, _, _) => Abs(t.erase, Metadata.none, "")
        case App(t1, t2, _) => App(t1.erase, t2.erase, Metadata.none)
        case Typ(_) => Typ(Metadata.none)
        case Phi(_) => Phi(Metadata.none)
        case Pro(t1, t2, _, _) => Pro(t1.erase, t2.erase, Metadata.none, "")
        case Inf(t1, t2, _, _) => Pro(t1.erase, t2.erase, Metadata.none, "")
        case Imp(t1, t2, _) => Imp(t1.erase, t2.erase, Metadata.none)
        case Module(fields, _) => Module(fields.map(_.onTerm(_.erase)), Metadata.none)
        case Interface(fields, _) => Interface(fields.map(_.onTerm(_.erase)), Metadata.none)
        case Get(t, field, _) => Get(t.erase, field, Metadata.none)
        case As(te, ty, _) => As(te.erase, ty.erase, Metadata.none)
        case NatType(_) => NatType(Metadata.none)
        case Nat(value, _) => Nat(value, Metadata.none)
        case Succ(t, _) => Succ(t.erase, Metadata.none)
        case Debug(t, _) => Debug(t.erase, Metadata.none)
        case Match(t, onZero, onSucc, _, _) => Match(t.erase, onZero.erase, onSucc.erase, Metadata.none, "")

    /** evaluates `this` using the _call-by-value_ strategy */
    def eval: Term = this match
        case Term.App(t1, t2, _) => (t1.eval, t2.eval) match
            case (Term.Abs(body, _, _), arg) => (body.replace(0, arg >> 1) << 1).eval
            case (abs, arg) => Term.App(abs, arg, this.meta)
        case Term.Get(t, field, _) => t.eval match
            case mod @ Module(fields, _) => 
                fields.find(_.name == field) match
                    case Some(ModElem(_, value, _)) => (value.replace(0, mod >> 1) << 1).eval
                    case _ => Term.Get(mod, field, this.meta)
            case e => Term.Get(e, field, this.meta)
        case Term.As(te, ty, _) => te
        case Var(x, Some(t), _) => t
        case Succ(e, r) => e.eval match
            case Nat(value, _) => Nat(value + 1, this.meta)
            case u => Succ(u, r)
        case Debug(e, r) => e.eval match
            case Nat(value, _) =>
                println(s"debug $value") 
                Nat(value, this.meta)
            case u => Debug(u, r)
        case Match(e, onZero, onSucc, _, x) => e.eval match
            case Nat(0, _) => onZero.eval
            case Nat(other, _) => App(Abs(onSucc, onSucc.meta, x), Nat(other - 1, e.meta), onSucc.meta).eval
            case Succ(e, r) => App(Abs(onSucc, onSucc.meta, x), e, onSucc.meta).eval
            case other => Match(other, onZero, onSucc, this.meta, x)
        case _ => this

    def revert(ctx: C): Program = this match
        case Term.Var(x, tagged, r) => Program.Var(ctx(x), r)
        case Term.Abs(t, r, x) => 
            val (y, c) = put(ctx, x)
            Program.Abs(y, t.revert(c), r)
        case Term.App(t1, t2, r) => Program.App(t1.revert(ctx), t2.revert(ctx))
        case Term.Typ(r) => Program.Typ(r)
        case Term.Phi(r) => Program.Phi(r)
        case Term.Pro(t1, t2, r, "") => Program.Pro(None, t1.revert(ctx), t2.revert(ctx >> 1), r)
        case Term.Pro(t1, t2, r, x) => 
            val (y, c) = put(ctx, x)
            Program.Pro(Some(y), t1.revert(ctx), t2.revert(c), r)
        case Term.Inf(t1, t2, r, x) => 
            val (y, c) = put(ctx, x)
            Program.Inf(y, t1.revert(ctx), t2.revert(c), r)
        case Term.Imp(t1, t2, r) => Program.Imp(t1.revert(ctx), t2.revert(ctx >> 1), r)
        case Term.Module(fields, r) => 
            Program.Module(fields.map {
            case Term.ModElem(name, term, Term.Mode.Exp) => Program.ModElem(name, term.revert(put(ctx, "this")(1)), Program.Mode.Exp)
            case Term.ModElem(name, term, Term.Mode.Imp) => Program.ModElem(name, term.revert(put(ctx, "this")(1)), Program.Mode.Imp)
        })
        case Term.Interface(fields, r) => Program.Interface(fields.map {
            case Term.IntElem(name, term, Term.Mode.Exp) => Program.IntElem(name, term.revert(put(ctx, "this")(1)), Program.Mode.Exp)
            case Term.IntElem(name, term, Term.Mode.Imp) => Program.IntElem(name, term.revert(put(ctx, "this")(1)), Program.Mode.Imp)
        })
        case Term.Get(t, field, r) => Program.Get(t.revert(ctx), field, r)
        case Term.As(te, ty, r) => Program.As(te.revert(ctx), ty.revert(ctx))
        case Term.NatType(r) => Program.NatType(r)
        case Term.Nat(value, r) => Program.Nat(value, r)
        case Term.Succ(t, r) => Program.Succ(t.revert(ctx), r)
        case Term.Debug(t, r) => Program.Debug(t.revert(ctx), r)
        case Term.Match(t, onZero, onSucc, r, x) => 
            val (y, c) = put(ctx, x)
            Program.Match(t.revert(ctx), onZero.revert(ctx), Program.Abs(y, onSucc.revert(c), r), r)


    /** alias for [[equivalence this.equivalence(that, Set.empty)]]*/
    def ===(that: Term): Boolean = this.equivalence(that, Set.empty)

    /** checks coinductively, whether `this` and `that` are behaviourally equivalent.
      * It terminates even for terms that would have infinite size with reduced redices.
      * It does not terminate, if [[eval]] does not terminate on a subterm of either `this` or `that`
      * @param that another term
      * @param relation coinduction hypothesis
      * @return whether this and that behave the same under evaluation
      */
    def equivalence(that: Term, relation: R): Boolean = 
        if relation(this, that) || this == that then true else
            def updatedRelation = relation + ((this, that))
            (this.eval, that.eval) match
                case (Var(x, _, _), Var(y, _, _)) => x == y
                case (Abs(t, _, _), Abs(e, _, _)) => t.equivalence(e, updatedRelation)
                case (App(t1, t2, _), App(e1, e2, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (Typ(_), Typ(_)) => true
                case (Phi(_), Phi(_)) => throw new Exception("equivalence should only be called after implicit resolution")
                case (Pro(t1, t2, _, _), Pro(e1, e2, _, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (Imp(t1, t2, _), Imp(e1, e2, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (Module(ts, _), Module(es, _)) => (ts zip es).forall {
                    case (ModElem(s1, t1, m1), ModElem(s2, t2, m2)) => s1 == s2 && m1 == m2 && t1.equivalence(t2, updatedRelation)
                }
                case (Interface(ts, _), Interface(es, _)) => (ts zip es).forall {
                    case (IntElem(s1, t1, m1), IntElem(s2, t2, m2)) => s1 == s2 && m1 == m2 && t1.equivalence(t2, updatedRelation)
                }
                case (As(t1, t2, _), As(e1, e2, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (Get(t, f1, _), Get(e, f2, _)) => f1 == f2 && t.equivalence(e, updatedRelation)
                case (NatType(_), NatType(_)) => true
                case (Nat(v1, _), Nat(v2, _)) => v1 == v2
                case (Succ(t1, _), Succ(t2, _)) => t1.equivalence(t2, updatedRelation)
                case (Debug(t1, _), Debug(t2, _)) => t1.equivalence(t2, updatedRelation)
                case (Match(t1, z1, s1, _, _), Match(t2, z2, s2, _, _)) => t1.equivalence(t2, updatedRelation) && z1.equivalence(z2, updatedRelation) && s1.equivalence(s2, updatedRelation)
                case _ => false

    /** checks, if this (seen as a type) conforms to the expected shape using [[===]]
      * @param shape optional type
      * @return `this === shape.get`, if shape is defined or else true
      */
    def matches(shape: Option[Term]): Boolean = 
        //println(s"DEBUG: $this should match $shape")
        shape match
            case Some(value) => this === value
            case None => true

    extension (self: Option[List[(Int, Term)]]) {
        def &&(that: Option[List[(Int, Term)]]) = for
            a <- self
            b <- that
        yield a ::: b
        def >>(amount: Int): Option[List[(Int, Term)]] = self.map(_ >> amount)
        def <<(amount: Int): Option[List[(Int, Term)]] = self.map(_ << amount)
    }

    extension (self: List[Int]) {
        @targetName("shiftListInt")
        def >>(amount: Int) = self.map(_ + amount)
    }

    extension (self: List[(Int, Term)]) {
        @targetName("shiftListIntTerm")
        def >>(amount: Int): List[(Int, Term)] = self.map((i, t) => (i + amount, t >> amount))
        @targetName("shiftListIntTermDown")
        def <<(amount: Int): List[(Int, Term)] = self.map((i, t) => (i - amount, t << amount))
    }

    /** searches a term of type `typ` in the implicit scope `all`. The term is constructed inductively, if implicit function are found in the implicit scope.
     * This method resembles the automatic proof strategy of the compiler.
      * @param typ expected type of the synthesized implicit
      * @param i not searched implicit scope
      * @param all the complete implicit scope
      * @return the synthesized term of type `typ`
      */
    def search(typ: Term, i: I): Option[Term] = 
        searchDirect(typ, i, i).orElse(searchFunction(typ, i, i))

    def searchDirect(typ: Term, i: I, all: I): Option[Term] = i.headOption match
        case Some(imp, index) if imp === typ => Some(index)
        case Some(_) => searchDirect(typ, i.tail, all)
        case None => None

    def searchFunction(typ: Term, i: I, all: I): Option[Term] = i.headOption match
        case Some((Imp(t1, t2, _), i)) if t2 === (typ >> 1) => search(t1, all).map { arg => App(i, arg, this.meta) }
        case Some(_) => searchFunction(typ, i.tail, all)
        case None => None

    def checkWithSearch(ty: Term, shape: Option[Term], i: I, c: C, g: G) = shape match
        case None => Result.succeed(this, ty)
        case Some(value) => resolveAll(this, ty, value, g, i, c)

    /** Checks that `ty` conforms to `shape` up to implicit resolution.
      * Uses [[matches]] and [[search]] to synthesize a term such that `ty` matches `shape`. 
      * If `ty` does not match `shape`, but is an implicit function type, then the implicit scope `i` is accessed.
      * @param ty type of `this`
      * @param shape expected type of `this`
      * @param i the implicit scope
      * @return type-checking result
      */
    // def checkWithSearch(ty: Term, shape: Option[Term], i: I, c: C, g: G): Result[TypeError, (Term, Term)] =
    //     val res = 
    //         if ty matches shape 
    //         then Result.Success(this, shape.getOrElse(ty)) 
    //         else Result.fail(TypeError.Mismatch(shape.get.revert(c), ty.revert(c), this.meta))
    //     res.orElse { (ty, shape) match
    //         case (Imp(t1, t2, _), shape) =>
    //             search(t1, i) match
    //                 case None => Result.fail(TypeError.Mismatch(shape.get.revert(c), ty.revert(c), this.meta))
    //                 case Some(arg) => checkWithSearch(t2, shape, i, c, g).map { (te, ty) => (App(te, arg, this.meta), ty) }
    //         case _ => Result.fail(TypeError.Mismatch(shape.get.revert(c), ty.revert(c), this.meta))
    //     }.orElse { shape match
    //         case Some(shape) if ty.isInstanceOf[Inf] => inferAll(ty, shape, c, g)
    //         case _ => if ty.isInstanceOf[Inf]
    //             then Result.fail(TypeError.Message("shape neccessary for infering type", this.meta))
    //             else Result.fail(TypeError.Mismatch(shape.get.revert(c), ty.revert(c), this.meta))
    //     }

    def searchAll(ty: Term, i: I, c: C): Result[TypeError, (Term, Term)] = ty match
        case Imp(t1, t2, _) => search(t1, i) match
            case Some(value) =>
                this.searchAll(t2, i, c).map { (thi, thy) => (App(thi, value, this.meta), thy) }
            case None => Result.fail(TypeError.NoImplicitFound(Some(t1.revert(c)), this.meta))
        case other => Result.succeed(this, ty)

    // def collectVars(ty: Term): (List[Int], Term) = ty match
    //     case Inf(t1, t2, r, x) => 
    //         val (xs, t) = t2.collectVars(t2)
    //         (0 :: (xs.map(_ + 1)), t)
    //     case _ => (Nil, ty)

    // def inferAll(ty: Term, shape: Term, c: C, g: G): Result[TypeError, (Term, Term)] =
    //     // all [x] variables and the typ after them
    //     val (vars, typ) = collectVars(ty)
    //     typ.genEq(shape >> (vars.max + 1), Set.empty, vars) match
    //         case None => 
    //             Result.fail(TypeError.Mismatch(shape.revert(c), ty.revert(c), this.meta))
    //         case Some(value) => validateGenEqRes(value) match
    //             case None => Result.fail(TypeError.Message("failed to match all constraints", this.meta))
    //             case Some(value) => 
    //                 val notInferred = vars.filter(v => value.forall(_(0) != v))
    //                 if notInferred.nonEmpty
    //                 then Result.fail(TypeError.Message(s"no constraint for $notInferred", this.meta))
    //                 else Result.succeed(this, shape)

    // def inferPartialForApp(funType: Term, fun: Term, argType: Term, arg: Term, i: I, c: C, g: G, shape: Option[Term]): Result[TypeError, (Term, Term)] = 
    //     // get variables that could be inferred and the type in which the inference will happen
    //     val (vars, innerType) = collectVars(funType)
    //     fun.searchAll(innerType, i, c).flatMap {
    //         case (abs, Pro(input, output, r, x)) => 
    //             input.genEq(argType >> (vars.max + 1), Set.empty, vars) match
    //                 case None => Result.fail(TypeError.Message("failed in inferAll(): genEq returned None", this.meta))
    //                 case Some(value) => validateGenEqRes(value) match
    //                     case None => Result.fail(TypeError.Message("failed to match all constraints", this.meta))
    //                     case Some(inferredVals) => // inference worked :)
    //                         // we need to reconstruct funType with inferredVals
    //                         def reconstruct(ty: Term, currentVar: Int): Term = ty match
    //                             case Inf(t1, t2, r, x) => 
    //                                 if inferredVals.contains(currentVar) 
    //                                 then reconstruct(t2.replace(0, inferredVals(currentVar) << currentVar) << 1, currentVar - 1)
    //                                 else Inf(t1, reconstruct(t2, currentVar - 1), r, x)
    //                             case Pro(u1, u2, r2, _) => u2.replace(0, arg >> 1) << 1 // shifting not defined
    //                             case _ => throw new Exception("cant happen!")
    //                             // println(inferredVals.size)
    //                             App(abs, arg, this.meta).checkWithSearch(reconstruct(funType, vars.max), shape, i, c, g)
    //         case e => Result.fail(TypeError.Message(s"product type expected! [[$e]](in partialForApp)", this.meta))
    //     }

    def genEq(that: Term, relation: R, variables: List[Int]): Option[List[(Int, Term)]] =
        // println(s"genEq [$this] === [$that]  ---- ${this.getClass} / ${that.getClass}")

        if relation(this, that) then Some(Nil) else
            // relation with assumed equivalence of this and right
            def updatedRelation = relation + ((this, that))
            (this.eval, that.eval) match
                case (Var(x, _, _), term) =>
                    val res = if variables contains x then Some((x, term) :: Nil)
                              else if term.isInstanceOf[Var] && term.asInstanceOf[Var].x == x then Some(Nil)
                              else None
                    println(res)
                    res
                    // Option.when(variables contains x)((x, term) :: Nil)
                case (Abs(t, _, _), Abs(e, _, _)) =>
                    t.genEq(e, updatedRelation, variables >> 1) << 1
                case (App(t1, t2, _), App(e1, e2, _)) =>
                    t1.genEq(e1, updatedRelation, variables) && t2.genEq(e2, updatedRelation, variables)
                case (Typ(_), Typ(_)) => Some(Nil)
                case (Phi(_), Phi(_)) => throw new Exception("equivalence should only be called after implicit resolution")
                case (Pro(t1, t2, _, _), Pro(e1, e2, _, _)) =>
                    t1.genEq(e1, updatedRelation, variables) && (t2.genEq(e2, updatedRelation, variables >> 1) << 1)
                case (Imp(t1, t2, _), Imp(e1, e2, _)) =>
                    t1.genEq(e1, updatedRelation, variables) && (t2.genEq(e2, updatedRelation, variables >> 1) << 1)
                case (Module(ts, _), Module(es, _)) =>
                    (ts zip es).foldLeft(Option(List.empty[(Int, Term)])) {
                        case (opt, (ModElem(s1, t1, m1), ModElem(s2, t2, m2))) => 
                            opt.flatMap { xs => 
                                (t1.genEq(t2, updatedRelation, variables >> 1) << 1).flatMap { ys =>
                                    Option.when(s1 == s2 && m1 == m2)(xs ++ ys)
                                }
                            }
                    }
                case (Interface(ts, _), Interface(es, _)) =>
                    (ts zip es).foldLeft(Option(List.empty[(Int, Term)])) {
                    case (opt, (IntElem(s1, t1, m1), IntElem(s2, t2, m2))) => 
                            opt.flatMap { xs => 
                                (t1.genEq(t2, updatedRelation, variables >> 1) << 1).flatMap { ys =>
                                    Option.when(s1 == s2 && m1 == m2)(xs ++ ys)
                                }
                            }
                    }
                case (As(t1, t2, _), As(e1, e2, _)) => t1.genEq(e1, updatedRelation, variables) && t2.genEq(e2, updatedRelation, variables)
                case (Get(t, f1, _), Get(e, f2, _)) => t.genEq(e, updatedRelation, variables).filter(_ => f1 == f2)
                case (NatType(_), NatType(_)) => Some(Nil)
                case (Nat(v1, _), Nat(v2, _)) => Option.when(v1 == v2)(Nil)
                case (Succ(t1, _), Succ(t2, _)) => t1.genEq(t2, updatedRelation, variables)
                case (Debug(t1, _), Debug(t2, _)) => t1.genEq(t2, updatedRelation, variables)
                case (Match(t1, z1, s1, _, _), Match(t2, z2, s2, _, _)) => t1.genEq(t2, updatedRelation, variables) && z1.genEq(z2, updatedRelation, variables) && (s1.genEq(s2, updatedRelation, variables >> 1) << 1)
                case _ => None

    def validateGenEqRes(res: List[(Int, Term)]) = 
            val map = res.groupBy(_(0))
            if map.forall { 
                case (i, (_, x) :: xs) => xs.forall(_(1) === x)
                case (i, _) => true // can't happen
            } then Some(map.map(_(1).head)) else None

    def unify(one: Map[Int, Term], two: Map[Int, Term]): Result[TypeError, Map[Int, Term]] = 
        two.foldLeft[Result[TypeError, Map[Int, Term]]](Result.succeed(one)) {
            case (res @ Result.Success(map), (v -> t)) => map.get(v) match
                case Some(u) => if t === u then res else Result.fail(TypeError.Message("constraints cannot be unified", this.meta))
                case None    => Result.succeed(map + (v -> t))
            case (fail, _) => fail
        }

    def resolve(reqs: List[Req], t: Term, u: Term, g: G, i: I, c: C): Result[TypeError, Map[Int, Term]] =
        val vars = List.range(0, reqs.length).filter(i => reqs(i).isInstanceOf[Req.Inf])
        t.genEq(u >> reqs.length, Set.empty, vars) match
            case None => Result.fail(TypeError.Message("t and u are unequal in resolve(reqs, t, u)", this.meta))
            case Some(result) => validateGenEqRes(result) match
                case None => Result.fail(TypeError.Message("constraints cannot be unified", this.meta))
                case Some(constraints) =>
                    vars.foldLeft[Result[TypeError, Map[Int, Term]]](Result.succeed(constraints)) { 
                        case (res @ Result.Success(map), v) => reqs(reqs.length - 1 - v) match
                            case Req.Inf(t) => map.get(v) match
                                case Some(value) =>
                                    (value << reqs.length).transform(g, i, c, None).flatMap { (_, typ) =>
                                        t.genEq(typ >> reqs.length - 1 - v, Set.empty, vars)
                                         .flatMap(validateGenEqRes)
                                         .map(cons => unify(map, cons.map((i, t) => (i - (reqs.length - v), t << reqs.length - v))))
                                         .getOrElse(Result.fail(TypeError.Message("constraints cannot be unified", this.meta)))
                                    }
                                case None => res
                            case Req.Imp(t) => res
                        case (fail, _) => fail
                    }

    def requirements: (List[Req], Term) = this match
        case Inf(t1, t2, meta, x) => 
            val (xs, t) = t2.requirements
            (Req.Inf(t1) :: xs, t)
        case Imp(t1, t2, meta) =>
            val (xs, t) = t2.requirements
            (Req.Imp(t1) :: xs, t)
        case other => (Nil, other)

    def resolveAll(te: Term, ty: Term, shape: Term, g: G, i: I, c: C): Result[TypeError, (Term, Term)] = 
        val (reqs, core) = ty.requirements
        resolve(reqs, core, shape, g, i, c).flatMap { map =>
            // reconstruct by using map and implicit search
            def reconstruct(te: Term, ty: Term, v: Int): Result[TypeError, (Term, Term)] = ty match
                case Inf(t1, t2, r, x) => 
                    if map.contains(v)
                    then reconstruct(te, t2.replace(0, map(v) << v) << 1, v - 1)
                    else reconstruct(te, t2, v - 1).map { (newTe, newTy) => (newTe, Inf(t1, newTy, r, x)) }
                case Imp(t1, t2, meta) =>
                    search(te, i).map { imp =>
                        reconstruct(te, t2, v - 1).map { (newTe, newTy) => 
                            (App(newTe, imp, this.meta), newTy)
                        }
                    }.getOrElse(Result.fail(TypeError.Message("No implicit found", this.meta)))
                case other => Result.succeed(te, other)

            reconstruct(te, ty, reqs.length - 1).flatMap { (te, ty) =>
                if ty === shape
                then Result.Success(this, shape) 
                else Result.fail(TypeError.Mismatch(shape.revert(c), ty.revert(c), this.meta))
            }
        }

    def resolveApp(fun: Term, arg: Term, funType: Term, argType: Term, g: G, i: I, c: C): Result[TypeError, (Term, Term)] =
        val (reqs, core) = funType.requirements
        core match
            case Pro(in, out, meta, x) => 
                resolve(reqs, in, argType, g, i, c).flatMap { map =>

                    // reconstruct by using map and implicit search
                    def reconstruct(te: Term, ty: Term, v: Int): Result[TypeError, (Term, Term)] = ty match
                        case Inf(t1, t2, r, x) => 
                            if map.contains(v) 
                            then reconstruct(te, t2.replace(0, map(v) << v) << 1, v - 1)
                            else reconstruct(te, t2, v - 1).map { (newTe, newTy) => (newTe, Inf(t1, newTy, r, x)) }
                        case Imp(t1, t2, meta) =>
                            search(te, i).map { imp =>
                                reconstruct(te, t2, v - 1).map { (newTe, newTy) => 
                                    (App(newTe, imp, this.meta), newTy)
                                }
                            }.getOrElse(Result.fail(TypeError.Message("No implicit found", this.meta)))

                        case Pro(u1, u2, r2, _) => Result.succeed(te, u2.replace(0, arg >> 1) << 1)
                        case _ => throw new Exception("cant happen!")

                    reconstruct(App(fun, arg, this.meta), funType, reqs.length - 1)

                }
            case e => Result.fail(TypeError.Message(s"product type expected! [[$e]](in partialForApp)", this.meta))

    /** Generate the type of `this` and an expected `shape` of the type, while simultaneously resolving implicits.
      * @param g explicit scope
      * @param i implicit scope
      * @param shape expected shape of the type
      * @return either term and type with resolved implicits or an error message
      */
    def transform(g: G, i: I, c: C, shape: Option[Term]): Result[TypeError, (Term, Term)] =
        // typecheck and transform the shape MOVED TO CALL SITE
        // if this.isInstanceOf[Typ] && shape.exists(_.isInstanceOf[Typ]) then Result.Success(Typ(this.meta), Typ(this.meta))
        // else shape.map(_.transform(g, i, c, Some(Typ(this.meta)))
        shape.map(_.eval) match
            // add implicit in context // SHOULD t1 be checked to be nonempty ?
            case Some(Imp(t1, t2, r)) =>
                t1.transform(g, i, c, Some(Typ(this.meta))).flatMap((t1te, _) =>
                    val (y, ctx) = put(c, "imp$")
                    (this >> 1).transform(g >> 1, (i >> 1) + ((t1te >> 1) -> Var(0, None, this.meta)), ctx, Some(t2)).map { (te, ty) => 
                        (Abs(te, te.meta, y), Imp(t1te, ty, r))
                    }
                )
            case Some(Inf(t1, t2, r, x)) => 
                // valid
                println("gen inf")
                t1.transform(g, i, c, Some(Typ(this.meta))).flatMap((t1te, _) =>
                    val (y, ctx) = put(c, x)
                    // BEGIN critical part
                    (this >> 1).transform((g >> 1) + (0 -> (t1te >> 1)), i >> 1, ctx, Some(t2)).map { (te, ty) => 
                        (te << 1, Inf(t1te, ty, r, x)) // TODO is the downshift valid??
                    }
                    // END critical part
                )
            // case Some(Result.Failure(value)) => Result.Failure(value)
            // otherwise match on this and apply typechecking rules
            case shape          => this match
                // checks if Typ matches shape
                case Typ(r) => Typ(r).checkWithSearch(Typ(r), shape, i, c, g)
                // get type from context
                case Var(x, tag, r) => g get x match
                    // checks if variable matches shape
                    case Some(value) => Var(x, tag, r).checkWithSearch(value, shape, i, c, g)
                    // report undefined variable type
                    case None => 
                        // throw new Exception("crash")
                        Result.fail(TypeError.Undefined(x.toString, this.meta))
                // use shape to generate type for abstraction
                case Abs(t, r1, x1) => shape match       // Done WILL SHAPE BE NORMAL-FORM? I think not e. g. because of t2  (... evaluate with implicits inserted !!)
                    case Some(Pro(t1, t2, r2, x2)) =>    // Done SHOULD t1 be checked to be nonempty
                        val (y, ctx) = put(c, x1)
                        (t.transform((g >> 1) + (0 -> (t1 >> 1)), i >> 1, ctx, Some(t2))).map { (te, t3) =>
                            (Abs(te, r1, y), Pro(t1, t3, r2, x2))
                        }
                    case Some(other) => Result.fail(TypeError.Unexpected(s"product type", this.meta))
                    case _ => Result.fail(TypeError.Unexpected("product type", this.meta))

                case App(t1, t2, r1) =>
                    t1.transform(g, i, c, None).flatMap { (te, ty) =>
                        te.searchAll(ty, i, c).flatMap {
                            case (abs, Pro(u1, u2, r2, _)) =>
                                t2.transform(g, i, c, Some(u1)).flatMap { (t2te, t2ty) =>
                                    App(abs, t2te, r1).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i, c, g)
                                }
                            case (abs, typ) => 
                                t2.transform(g, i, c, None).flatMap { (t2te, t2ty) =>
                                    resolveApp(abs, t2te, typ, t2ty, g, i, c).flatMap { (resTe, resTy) =>
                                        resTe.checkWithSearch(resTy, shape, i, c, g)
                                    }
                                }
                        }
                    }

                // SOMEWHERE MISSING SHIFT
                // case App(t1, t2, meta) => t1.transform(g, i, c, None).flatMap { (te, ty) =>
                //     resolveApp(t1, t2, ty, ???, g, i, c)

                // }

                // case App(t1, t2, r1) => 
                //     t1.transform(g, i, c, None).flatMap { (te, ty) =>
                //         if ty.isInstanceOf[Inf] then
                //             t2.transform(g, i, c, None).flatMap { (t2te, t2ty) =>
                //                 inferPartialForApp(ty, te, t2ty, t2te, i, c, g, shape)
                //             }
                //         else
                //             te.searchAll(ty, i, c).flatMap {
                //             case (abs, Pro(u1, u2, r2, _)) =>
                //                 t2.transform(g, i, c, Some(u1)).flatMap { (t2te, t2ty) =>
                //                     App(abs, t2te, r1).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i, c, g)
                //                 }
                //             case (_, _) => Result.fail(TypeError.Unexpected("product type", this.meta))
                //         }
                //     }

                // case App(t1, t2, r1) => shape match
                //     case None =>
                //         t1.transform(g, i, c, None).flatMap { (te, ty) =>
                //         }
                //     case Some(shape) => ???

                // case App(t1, t2, r1) =>
                //     // typecheck t1
                //     t1.transform(g, i, c, None).flatMap { (te, ty) =>
                //         // resolve implicits
                //         te.searchAll(ty.eval, i, c).flatMap {
                //             // no inference necessary
                //             case (abs, Pro(u1, u2, r2, _)) =>
                //                 t2.transform(g, i, c, Some(u1)).flatMap { (t2te, t2ty) =>
                //                     App(abs, t2te, r1).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i, c)
                //                 }
                //             // inference necessary
                //             case (abs, inf: Inf) =>
                //                 // typecheck t2
                //                 t2.transform(g, i, c, None).flatMap { (t2te, t2ty) =>
                //                         val (vars, innerType) = collectVars(inf)
                //                         innerType.eval match
                //                             case Pro(u1, u2, r, x) => 
                //                                 u1.genEq(t2ty, Set.empty, vars).flatMap(validateGenEqRes) match
                //                                     case Some(value) => 
                //                                         val returnType = value.foldLeft(u2) {
                //                                             case (target, (v, te)) => target.replace(v, te)
                //                                         }
                //                                         App(abs, t2te, r1).checkWithSearch((returnType.replace(0, t2te >> 1) << 1), shape, i, c)

                //                                     case None => Result.fail(TypeError.Message("failed sadly :(", this.meta))
                //                             case _ => Result.fail(TypeError.Unexpected("product type (during inference)", this.meta))

                //                     // case Some(value) =>
                //                     //     // resolve implicits
                //                     //     abs.inferAll(inf, Pro(t2ty, value, this.meta, ""), c).flatMap { (_, _) =>
                //                     //         App(abs, t2te, r1).checkWithSearch((value.replace(0, t2te >> 1) << 1), shape, i, c)
                //                     //     }

                //                 }
                //             case (_, _) => Result.fail(TypeError.Unexpected("product type", this.meta))
                //         }
                //     }

                // case App(t1, t2, r1) =>
                //     // typecheck t1
                //     t1.transform(g, i, c, None).flatMap { (te, ty) =>
                //         if ty.isInstanceOf[Inf] then
                //             // typecheck t2
                //             t2.transform(g, i, c, None).flatMap { (t2te, t2ty) =>
                //                 if shape.isDefined then
                //                     te.inferAll(ty, Pro(t2ty, shape.get, this.meta, ""), c)
                //                 else ???
                //                 App(abs, t2te, r1).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i, c)
                //             }
                //             // inferAll()
                //         else
                //             te.searchAll(ty, i, c).flatMap {
                //                 case (abs, Pro(u1, u2, r2, _)) =>
                //                     t2.transform(g, i, c, Some(u1)).flatMap { (t2te, t2ty) =>
                //                         App(abs, t2te, r1).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i, c)
                //                     }
                //                 case (_, _) => Result.fail(TypeError.Unexpected("product type", this.meta))
                //             }
                //     }


                // parameter and result should be a type
                case Pro(t1, t2, r1, x) =>
                    t1.transform(g, i, c, Some(Typ(r1))).flatMap { (u1, _) =>
                        val (y, ctx) = put(c, x)
                        t2.transform((g >> 1) + (0 -> (t1 >> 1)), i >> 1, ctx, Some(Typ(r1))).flatMap { (u2, _) =>
                            Pro(u1, u2, r1, y).checkWithSearch(Typ(r1), shape, i, c, g)
                        }
                    }
                case Inf(t1, t2, r1, x) =>
                    t1.transform(g, i, c, Some(Typ(r1))).flatMap { (u1, _) =>
                        val (y, ctx) = put(c, x)
                        t2.transform((g >> 1) + (0 -> (t1 >> 1)), i >> 1, ctx, Some(Typ(r1))).flatMap { (u2, _) =>
                            Inf(u1, u2, r1, y).checkWithSearch(Typ(r1), shape, i, c, g)
                        }
                    }
                // search for implicit to match shape
                case Phi(_) => shape match
                    case Some(value) => 
                        //println(s"CONTEXT $i SHAPE $value")
                        // throw new Exception()
                        search(value, i).map(Result.Success(_, value)).getOrElse(Result.fail(TypeError.NoImplicitFound(shape.map(_.revert(c)), this.meta)))
                    case None => Result.fail(TypeError.NoImplicitFound(shape.map(_.revert(c)), this.meta))
                // parameter and result should be a type
                case Imp(t1, t2, r) =>
                    t1.transform(g, i, c, Some(Typ(t1.meta))).flatMap { (u1, _) =>
                        t2.transform(g >> 1, (i >> 1) + (t1 -> Var(0, None, this.meta)), (c >> 1), Some(Typ(r))).flatMap { (u2, _) =>
                            Imp(u1, u2, r).checkWithSearch(Typ(this.meta), shape, i, c, g)
                        }
                    }
                case As(te, ty, r) => 
                    ty.transform(g, i, c, Some(Typ(r))).flatMap { (ty, _) =>
                            te.transform(g, i, c, Some(ty)).flatMap { (ue, uy) =>
                            As(ue, ty, r).checkWithSearch(uy, shape.orElse(Some(ty)), i, c, g)
                        }
                    }

                case Module(fields, _) => shape match
                    case None => checkModule(fields, Nil, Nil, g, i, c)
                    case Some(Interface(types, _)) => checkModuleWithInterface(fields.zip(types), Nil, Nil, g, i, c)
                    case Some(_) => Result.fail(TypeError.Unexpected("interface", this.meta))

                case Interface(fields, _) => checkInterface(fields, Nil, g, i, c).flatMap { (te, ty) =>
                    te.checkWithSearch(ty, shape, i, c, g)
                }

                case Get(t, field, r) => t.transform(g, i, c, None).flatMap { (te, ty) =>
                    te.searchAll(ty.eval, i, c).flatMap {
                        case (te, Interface(fields, _)) => 
                            fields.find(_.name == field) match
                                case Some(IntElem(name, typ, mode)) =>
                                    // println(s"$typ -- $shape [${g.mkString(", ")}]")
                                    Get(te, field, r).checkWithSearch(typ.replace(0, te >> 1) << 1, shape, i, c, g) // should `te` be shifted?
                                case _ => Result.fail(TypeError.NoField(t.revert(c), field, this.meta))
                        case (te, ty) =>
                            Result.fail(TypeError.NoField(t.revert(c), field, this.meta))
                    }
                }

                // searchAlls are missing !!
                case Nat(value, r) => if value >= 0 then Nat(value, r).checkWithSearch(NatType(r), shape, i, c, g) else Result.fail(TypeError.Message("natural numbers are >= 0", this.meta))
                case NatType(r) => Result.Success(NatType(r), Typ(r))
                case Succ(t, r) => t.transform(g, i, c, Some(NatType(r))).flatMap((te, ty) => Succ(te, r).checkWithSearch(NatType(r), shape, i, c, g))
                case Debug(t, r) => t.transform(g, i, c, Some(NatType(r))).flatMap((te, ty) => Debug(te, r).checkWithSearch(NatType(r), shape, i, c, g))
                case Match(t, onZero, onSucc, r, x) => t.transform(g, i, c, Some(NatType(r))).flatMap { (te, ty) =>
                    onZero.transform(g, i, c, shape).flatMap { (z, zt) => 
                        val (y, ctx) = put(c, x)
                        onSucc.transform((g >> 1) + (0 -> NatType(r)), i, ctx, shape.map(_ >> 1)).flatMap { (s, st) =>
                            if (zt >> 1) === st 
                            then Result.Success(Match(te, z, s, r, y), zt) 
                            else Result.fail(TypeError.Message(s"the branches have different types: $zt and $st", this.meta))
                        }
                    }
                }

    /** helper of [[transform]] for checking modules with expected interface */
    def checkModuleWithInterface(fields: List[(ModElem, IntElem)], module: List[ModElem], interface: List[IntElem], g: G, i: I, c: C): Result[TypeError, (Module, Interface)] = fields match
        case Nil => Result.Success(Module(module.reverse, this.meta), Interface(interface.reverse, this.meta))
        case (ModElem(name, term, mode), IntElem(name1, typ, mode1)) :: next if name == name1 && mode == mode1 => 
            val expContext = (g >> 1) + (0 -> (Interface(interface ++ fields.map(_(1)), this.meta) >> 1))
            val impContext = (i >> 1) ++ interface.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None, this.meta), name, this.meta)) }.toMap.tag(0, Module(module, this.meta) >> 1)
            val taggedTerm = term.tag(0, Module(module, this.meta) >> 1)
            val taggedType = typ.tag(0, Module(module, this.meta) >> 1)
            taggedTerm.transform(expContext, impContext, put(c, "this")(1), Some(taggedType)).flatMap { (te, ty) =>
                checkModuleWithInterface(next, ModElem(name, te, mode) :: module, IntElem(name, typ, mode) :: interface, g, i, c)
            }
        case (ModElem(_, _, teMode), IntElem(_, _, tyMode)) :: _ => Result.fail(TypeError.Message(s"found $teMode for $tyMode", this.meta))
    
    /** helper of [[transform]] for checking modules without expected interface */
    def checkModule(fields: List[ModElem], module: List[ModElem], interface: List[IntElem], g: G, i: I, c: C): Result[TypeError, (Module, Interface)] = fields match
        case Nil => Result.Success(Module(module.reverse, this.meta), Interface(interface.reverse, this.meta))
        case ModElem(name, term, mode) :: next => 
            val expContext = (g >> 1) + (0 -> (Interface(interface, this.meta) >> 1))
            val impContext = (i >> 1) ++ interface.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None, this.meta), name, this.meta)) }.toMap.tag(0, Module(module, this.meta) >> 1)
            val taggedTerm = term.tag(0, Module(module, this.meta) >> 1)
            taggedTerm.transform(expContext, impContext, put(c, "this")(1), None).flatMap { (te, ty) =>
                checkModule(next, ModElem(name, te, mode) :: module, IntElem(name, ty, mode) :: interface, g, i, c)
            }

    /** helper of [[transform]] for checking interfaces */
    def checkInterface(fields: List[IntElem], checked: List[IntElem], g: G, i: I, c: C): Result[TypeError, (Interface, Term.Typ)] = fields match
        case Nil => Result.Success(Interface(checked.reverse, this.meta), Typ(this.meta))
        case IntElem(name, typ, mode) :: next =>
            val expContext = (g >> 1) + (0 -> (Interface(fields ++ checked, this.meta)))
            val impContext =  (i >> 1) ++ checked.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None, this.meta), name, this.meta)) }
            typ.transform(expContext, impContext, put(c, "this")(1), Some(Typ(this.meta))).flatMap { (ty, _) => 
                checkInterface(next, IntElem(name, ty, mode) :: checked, g, i, c)
            }

    /** alias for [[transform transform(Map.empty, Map.empty, None)]] */
    def typeCheck = transform(Map.empty, Map.empty, Map.empty, None)

    /** alias for [[transform transform(Map.empty, Map.empty, Some(ty))]] */
    def typeCheck(ty: Term) = transform(Map.empty, Map.empty, Map.empty, Some(ty))

}