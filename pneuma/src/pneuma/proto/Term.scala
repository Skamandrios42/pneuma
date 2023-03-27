package pneuma.proto

import scala.annotation.targetName
import Term.{IntElem, ModElem, Mode}
import general.Result
import parsing.Parser.AttachRegion
import general.Region
import general.HasRegion

object Term {
    enum Mode { case Exp, Imp }

    case class ModElem(name: String, term: Term, mode: Mode) {
        override def toString: String = mode match
            case Mode.Exp => s"$name  = $term"
            case Mode.Imp => s"? $name = $term"
    }

    case class IntElem(name: String, term: Term, mode: Mode) {
        override def toString: String = mode match
            case Mode.Exp => s"$name : $term"
            case Mode.Imp => s"? $name : $term"
    }
}

/** The abstract syntax tree representation of a pneuma program 
  * Use [[DSL]] to define terms in _Scala_
  */
enum Term extends HasRegion {
    case Var(x: Int, tagged: Option[Term], r: Region)
    case Abs(t: Term, r: Region)
    case App(t1: Term, t2: Term, r: Region)
    case Typ(r: Region)
    case Phi(r: Region)
    case Pro(t1: Term, t2: Term, r: Region)
    case Imp(t1: Term, t2: Term, r: Region)
    case Module(fields: List[ModElem], r: Region)
    case Interface(fields: List[IntElem], r: Region)
    case Get(t: Term, field: String, r: Region)
    case As(te: Term, ty: Term, r: Region)
    case NatType(r: Region)
    case Nat(value: Int, r: Region)
    case Succ(t: Term, r: Region)
    case Debug(t: Term, r: Region)
    case Match(t: Term, onZero: Term, onSucc: Term, r: Region)

    def rOf(that: Term): Term = this match
        case Var(x, tagged, r) => Var(x, tagged, that.r)
        case Abs(t, r) => Abs(t, that.r)
        case App(t1, t2, r) => App(t1, t2, that.r)
        case Typ(r) => Typ(that.r)
        case Phi(r) => Phi(that.r)
        case Pro(t1, t2, r) => Pro(t1, t2, that.r)
        case Imp(t1, t2, r) => Imp(t1, t2, that.r)
        case Module(fields, r) => Module(fields, that.r)
        case Interface(fields, r) => Interface(fields, that.r)
        case Get(t, field, r) => Get(t, field, that.r)
        case As(te, ty, r) => As(te, ty, that.r)
        case NatType(r) => NatType(that.r)
        case Nat(value, r) => Nat(value, that.r)
        case Succ(t, r) => Succ(t, that.r)
        case Debug(t, r) => Debug(t, that.r)
        case Match(t, onZero, onSucc, r) => Match(t, onZero, onSucc, that.r)

    def eraseRegion: Term = this match
        case Var(x, tagged, _) => Var(x, tagged, Region(None, 0, 0))
        case Abs(t, _) => Abs(t, Region(None, 0, 0))
        case App(t1, t2, _) => App(t1, t2, Region(None, 0, 0))
        case Typ(_) => Typ(Region(None, 0, 0))
        case Phi(_) => Phi(Region(None, 0, 0))
        case Pro(t1, t2, _) => Pro(t1, t2, Region(None, 0, 0))
        case Imp(t1, t2, _) => Imp(t1, t2, Region(None, 0, 0))
        case Module(fields, _) => Module(fields, Region(None, 0, 0))
        case Interface(fields, _) => Interface(fields, Region(None, 0, 0))
        case Get(t, field, _) => Get(t, field, Region(None, 0, 0))
        case As(te, ty, _) => As(te, ty, Region(None, 0, 0))
        case NatType(_) => NatType(Region(None, 0, 0))
        case Nat(value, _) => Nat(value, Region(None, 0, 0))
        case Succ(t, _) => Succ(t, Region(None, 0, 0))
        case Debug(t, _) => Debug(t, Region(None, 0, 0))
        case Match(t, onZero, onSucc, _) => Match(t, onZero, onSucc, Region(None, 0, 0))
    

    /** generates a simple string representation of the program */
    override def toString: String = this match
        case Var(x, Some(t), _) => s"$x[$t]"
        case Var(x, None, _) => s"$x"
        case Abs(t, _) => s"(λ.$t)"
        case App(t1, t2, _) => s"($t1 $t2)"
        case Typ(_) => "*"
        case Phi(_) => "?"
        case Pro(t1, t2, _) => s"(π$t1.$t2)"
        case Imp(t1, t2, _) => s"(?$t1.$t2)"
        case Module(fields, _) => s"{ ${fields.mkString(", ")} }"
        case Interface(fields, _) => s"{ ${fields.mkString(", ")} }"
        case Get(t, field, _) => s"($t.$field)"
        case As(te, ty, _) => s"($te : $ty)"
        case NatType(_) => "Nat"
        case Nat(value, _) => s"n$value"
        case Succ(t, _) => s"(S $t)"
        case Debug(t, _) => s"(debug $t)"
        case Match(t, onZero, onSucc, _) => s"($t match Z => $onZero, S => $onSucc )"

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

    /** adds `amount` to all variables with distance of `cutoff` or longer
      * @param amount number to be added
      * @param cutoff border between shifted and not shifted variables
      * @return the same term, with shifted variables
      */
    def shift(amount: Int, cutoff: Int): Term = this match
        case Var(x, Some(t), _) => if x >= cutoff then Var(x + amount, Some(t.shift(amount, cutoff)), this.r) else Var(x, Some(t.shift(amount, cutoff)), this.r)
        case Var(x, None, _) => if x >= cutoff then Var(x + amount, None, this.r) else Var(x, None, this.r)
        case Abs(t, _) => Abs(t.shift(amount, cutoff + 1), this.r)
        case App(t1, t2, _) => App(t1.shift(amount, cutoff), t2.shift(amount, cutoff), this.r)
        case Typ(_) => Typ(this.r)
        case Phi(_) => Phi(this.r)
        case Pro(t1, t2, _) => Pro(t1.shift(amount, cutoff), t2.shift(amount, cutoff + 1), this.r)
        case Imp(t1, t2, _) => Imp(t1.shift(amount, cutoff), t2.shift(amount, cutoff), this.r)
        case Module(fields, _) => Module(fields.mapValues(_.shift(amount, cutoff + 1)), this.r)
        case Interface(fields, _) => Interface(fields.mapValues(_.shift(amount, cutoff + 1)), this.r)
        case Get(t, field, _) => Get(t.shift(amount, cutoff), field, this.r)
        case As(te, ty, _) => Term.As(te.shift(amount, cutoff), ty.shift(amount, cutoff), this.r)
        case NatType(r) => NatType(this.r)
        case Nat(value, _) => Nat(value, this.r)
        case Succ(t, _) => Succ(t.shift(amount, cutoff), this.r)
        case Debug(t, _) => Debug(t.shift(amount, cutoff), this.r)
        case Match(t, onZero, onSucc, _) => Match(t.shift(amount, cutoff), onZero.shift(amount, cutoff), onSucc.shift(amount, cutoff + 1), this.r)

    /** returns [[shift(amount, 0)]] */
    def >>(amount: Int) = shift(amount, 0)

    /** returns [[shift(-amount, 0)]] */
    def <<(amount: Int) = shift(-amount, 0)

    /** replaces all occurrences of a variable by `t`
      * @param y the variable to be replaces
      * @param t the term to be inserted
      * @return `this` with replaced variables
      */
    def replace(y: Int, t: Term): Term = this match
        case Var(x, Some(e), _) => if x == y then t.rOf(this) else Var(x, Some(e.replace(y, t)), this.r)
        case Var(x, None, _) => if x == y then t.rOf(this) else Var(x, None, this.r)
        case Abs(t1, _) => Abs(t1.replace(y + 1, t >> 1), this.r)
        case App(t1, t2, _) => App(t1.replace(y, t), t2.replace(y, t), this.r)
        case Typ(_) => Typ(this.r)
        case Phi(_) => Phi(this.r)
        case Pro(t1, t2, _) => Pro(t1.replace(y, t), t2.replace(y + 1, t >> 1), this.r)
        case Imp(t1, t2, _) => Imp(t1.replace(y, t), t2.replace(y + 1, t >> 1), this.r)
        case Module(fields, _) => Module(fields.mapValues(_.replace(y + 1, t >> 1)), this.r)
        case Interface(fields, _) => Interface(fields.mapValues(_.replace(y + 1, t >> 1)), this.r)
        case Get(t1, field, _) => Get(t1.replace(y, t), field, this.r)
        case As(te, ty, _) => Term.As(te.replace(y, t), ty.replace(y, t), this.r)
        case NatType(_) => NatType(this.r)
        case Nat(value, _) => Nat(value, this.r)
        case Succ(e, _) => Succ(e.replace(y, t), this.r)
        case Debug(e, _) => Debug(e.replace(y, t), this.r)
        case Match(e, onZero, onSucc, _) => Match(e.replace(y, t), onZero.replace(y, t), onSucc.replace(y + 1, t >> 1), this.r)

    /** similiar to [[replace]], but instead of replacing, `t` is added as in the `tagged` field of the variable
      * @param y the variable to be tagged
      * @param t the term to be inserted
      * @return `this` with tagged variables
      */
    def tag(y: Int, t: Term): Term = this match
        case Var(x, Some(e), _) => if x == y then Var(x, Some(t), this.r) else Var(x, Some(e.tag(y, t)), this.r)
        case Var(x, None, _) => if x == y then Var(x, Some(t), this.r) else Var(x, None, this.r)
        case Abs(t1, _) => Abs(t1.tag(y + 1, t >> 1), this.r)
        case App(t1, t2, _) => App(t1.tag(y, t), t2.tag(y, t), this.r)
        case Typ(_) => Typ(this.r)
        case Phi(_) => Phi(this.r)
        case Pro(t1, t2, _) => Pro(t1.tag(y, t), t2.tag(y + 1, t >> 1), this.r)
        case Imp(t1, t2, _) => Imp(t1.tag(y, t), t2.tag(y + 1, t >> 1), this.r)
        case Module(fields, _) => Module(fields.mapValues(_.tag(y + 1, t >> 1)), this.r)
        case Interface(fields, _) => Interface(fields.mapValues(_.tag(y + 1, t >> 1)), this.r)
        case Get(t1, field, _) => Get(t1.tag(y, t), field, this.r)
        case As(te, ty, _) => Term.As(te.tag(y, t), ty.tag(y, t), this.r)
        case NatType(_) => NatType(this.r)
        case Nat(value, _) => Nat(value, this.r)
        case Succ(e, _) => Succ(e.tag(y, t), this.r)
        case Debug(e, _) => Debug(e.tag(y, t), this.r)
        case Match(e, onZero, onSucc, _) => Match(e.tag(y, t), onZero.tag(y, t), onSucc.tag(y + 1, t >> 1), this.r)

    /** removes all tags from the variables */
    def untag: Term = this match
        case Var(x, _, _) => Var(x, None, this.r)
        case Abs(t1, _) => Abs(t1.untag, this.r)
        case App(t1, t2, _) => App(t1.untag, t2.untag, this.r)
        case Typ(_) => Typ(this.r)
        case Phi(_) => Phi(this.r)
        case Pro(t1, t2, _) => Pro(t1.untag, t2.untag, this.r)
        case Imp(t1, t2, _) => Imp(t1.untag, t2.untag, this.r)
        case Module(fields, _) => Module(fields.mapValues(_.untag), this.r)
        case Interface(fields, _) => Interface(fields.mapValues(_.untag), this.r)
        case Get(t1, field, _) => Get(t1.untag, field, this.r)
        case As(te, ty, _) => Term.As(te.untag, ty.untag, this.r)
        case NatType(_) => NatType(this.r)
        case Nat(value, _) => Nat(value, this.r)
        case Succ(e, _) => Succ(e.untag, this.r)
        case Debug(e, _) => Debug(e.untag, this.r)
        case Match(t, onZero, onSucc, _) => Match(t.untag, onZero.untag, onSucc.untag, this.r)
        

    /** evaluates `this` using the _call-by-value_ strategy */
    def eval: Term = this match
        case Term.App(t1, t2, _) => (t1.eval, t2.eval) match
            case (Term.Abs(body, _), arg) => (body.replace(0, arg >> 1) << 1).eval
            case (abs, arg) => Term.App(abs, arg, this.r)
        case Term.Get(t, field, _) => t.eval match
            case mod @ Module(fields, _) => 
                fields.find(_.name == field) match
                    case Some(ModElem(_, value, _)) => (value.replace(0, mod >> 1) << 1).eval
                    case _ => Term.Get(mod, field, this.r)
            case e => Term.Get(e, field, this.r)
        case Term.As(te, ty, _) => te
        case Var(x, Some(t), _) => t
        case Succ(e, _) => e.eval match
            case Nat(value, _) => Nat(value + 1, this.r)
            case e => e
        case Debug(e, _) => e.eval match
            case Nat(value, _) =>
                println(s"debug $value") 
                Nat(value, this.r)
            case e => e
        case Match(e, onZero, onSucc, _) => e.eval match
            case Nat(0, _) => onZero.eval
            case Nat(other, _) => App(Abs(onSucc, onSucc.r), Nat(other - 1, e.r), onSucc.r).eval
            case other => Match(other, onZero, onSucc, this.r)
        case _ => this

    /** maps indices to types */
    type G = Map[Int, Term]
    /** maps types to variabls or module accesses */
    type I = Map[Term, Term]
    /** relation between terms */
    type R = Set[(Term, Term)]

    extension (self: G) {
        /** shifts all elements in the collection by amount */
        @targetName("shiftG")
        def >>(amount: Int): G = self.map { case (key, value) => (key + amount, value >> amount) }
    }

    extension (self: I) {
        /** shifts all elements in the collection by amount */
        @targetName("shiftI")
        def >>(amount: Int): I = self.map { case (key, value) => (key >> amount, value >> amount) }
    }

    extension (self: R) {
        @targetName("shiftR")
        /** shifts all elements in the collection by amount */
        def >>(amount: Int): R = self.map { case (key, value) => (key >> amount, value >> amount) }
    }

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
        if relation(this, that) then true else
            // relation with assumed equivalence of this and right
            def updatedRelation = relation + ((this, that))
            (this.eval, that.eval) match
                case (Var(x, _, _), Var(y, _, _)) => x == y
                case (Abs(t, _), Abs(e, _)) => t.equivalence(e, updatedRelation >> 1)
                case (App(t1, t2, _), App(e1, e2, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (Typ(_), Typ(_)) => true
                case (Phi(_), Phi(_)) => throw new Exception("equivalence should only be called after implicit resolution")
                case (Pro(t1, t2, _), Pro(e1, e2, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation >> 1)
                case (Imp(t1, t2, _), Imp(e1, e2, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation >> 1)
                case (Module(ts, _), Module(es, _)) => (ts zip es).forall {
                    case (ModElem(s1, t1, m1), ModElem(s2, t2, m2)) => s1 == s2 && m1 == m2 && t1.equivalence(t2, updatedRelation >> 1)
                }
                case (Interface(ts, _), Interface(es, _)) => (ts zip es).forall {
                    case (IntElem(s1, t1, m1), IntElem(s2, t2, m2)) => s1 == s2 && m1 == m2 && t1.equivalence(t2, updatedRelation >> 1)
                }
                case (As(t1, t2, _), As(e1, e2, _)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (Get(t, f1, _), Get(e, f2, _)) => f1 == f2 && t.equivalence(e, updatedRelation)
                case (NatType(_), NatType(_)) => true
                case (Nat(v1, _), Nat(v2, _)) => v1 == v2
                case (Succ(t1, _), Succ(t2, _)) => t1.equivalence(t2, updatedRelation)
                case (Debug(t1, _), Debug(t2, _)) => t1.equivalence(t2, updatedRelation)
                case (Match(t1, z1, s1, _), Match(t2, z2, s2, _)) => t1.equivalence(t2, updatedRelation) && z1.equivalence(z2, updatedRelation) && s1.equivalence(s2, updatedRelation)
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

    /** searches a term of type `typ` in the implicit scope `all`. The term is constructed inductively, if implicit function are found in the implicit scope.
     * This method resembles the automatic proof strategy of the compiler.
      * @param typ expected type of the synthesized implicit
      * @param i not searched implicit scope
      * @param all the complete implicit scope
      * @return the synthesized term of type `typ`
      */
    def search(typ: Term, i: I, all: I): Option[Term] = 
        searchDirect(typ, i, all).orElse(searchFunction(typ, i, all))

    def searchDirect(typ: Term, i: I, all: I): Option[Term] = i.headOption match
        case Some(imp, index) if imp === typ => 
            //println("found implicit")
            Some(index)
        case Some(_) => searchDirect(typ, i.tail, all)
        case None => None

    def searchFunction(typ: Term, i: I, all: I): Option[Term] = i.headOption match
        case Some((Imp(t1, t2, _), i)) if t2 === (typ >> 1) => 
            //println(s"found implicit function $t1 -> $t2")
            search(t1, all, all).map { arg => App(i, arg, this.r) }
        case Some(_) => searchFunction(typ, i.tail, all)
        case None => None

    /** Checks that `ty` conforms to `shape` up to implicit resolution.
      * Uses [[matches]] and [[search]] to synthesize a term such that `ty` matches `shape`. 
      * If `ty` does not match `shape`, but is an implicit function type, then the implicit scope `i` is accessed.
      * @param ty type of `this`
      * @param shape expected type of `this`
      * @param i the implicit scope
      * @return type-checking result
      */
    def checkWithSearch(ty: Term, shape: Option[Term], i: I): Result[TypeError, (Term, Term)] =
        val res = 
            if ty matches shape 
            then Result.Success(this, shape.getOrElse(ty)) 
            else Result.fail(TypeError.Mismatch(shape.get, ty, this.r))
        res.orElse { (ty, shape) match
            case (Imp(t1, t2, _), shape) =>
                search(t1, i, i) match
                    case None => Result.fail(TypeError.Mismatch(shape.get, ty, this.r))
                    case Some(arg) => checkWithSearch(t2, shape, i).map { (te, ty) => (App(te, arg, this.r), ty) }
            case _ => Result.fail(TypeError.Mismatch(shape.get, ty, this.r))
        }

    def searchAll(ty: Term, i: I): Result[TypeError, (Term, Term)] = ty match
        case Imp(t1, t2, _) => search(t1, i, i) match
            case Some(value) => 
                this.searchAll(t2, i).map { (thi, thy) => (App(thi, value, this.r), thy) }
            case None => Result.fail(TypeError.NoImplicitFound(Some(t1)))
        case other => Result.succeed(this, ty)

    /** Generate the type of `this` and an expected `shape` of the type, while simultaneously resolving implicits.
      * @param g explicit scope
      * @param i implicit scope
      * @param shape expected shape of the type
      * @return either term and type with resolved implicits or an error message
      */
    def transform(g: G, i: I, shape: Option[Term]): Result[TypeError, (Term, Term)] =
        // shape match
        //     case None => println(s"[DEBUG] $this")
        //     case Some(value) => println(s"[DEBUG] $this : $value")
        // typecheck and transform the shape
        if this.isInstanceOf[Typ] && shape.exists(_.isInstanceOf[Typ]) then Result.Success(Typ(this.r), Typ(this.r))
        else shape.map(_.transform(g, i, Some(Typ(this.r))).map(_(0).eval)) match
            // add implicit in context // SHOULD t1 be checked to be nonempty ?
            case Some(Result.Success(Imp(t1, t2, r))) =>
                t1.transform(g, i, Some(Typ(this.r))).flatMap((t1te, _) =>
                    (this >> 1).transform(g >> 1, (i >> 1) + ((t1te >> 1) -> Var(0, None, this.r)), Some(t2)).map((te, ty) => (Abs(te, te.r), Imp(t1te, ty, r)))
                )
            case Some(Result.Failure(value)) => Result.Failure(value)
            // otherwise match on this and apply typechecking rules
            case e          => 
                val shape = e.collect {
                    case Result.Success(x) => x
                }
                this match
                // checks if Typ matches shape
                case Typ(r) => Typ(r).checkWithSearch(Typ(r), shape, i)
                // get type from context
                case Var(x, tag, r) => g get x match
                    // checks if variable matches shape
                    case Some(value) => Var(x, tag, r).checkWithSearch(value, shape, i)
                    // report undefined variable type
                    case None => Result.fail(TypeError.Undefined(x.toString))
                // use shape to generate type for abstraction
                case Abs(t, r1) => shape match       // Done WILL SHAPE BE NORMAL-FORM? I think not e. g. because of t2  (... evaluate with implicits inserted !!)
                    case Some(Pro(t1, t2, r2)) =>    // Done SHOULD t1 be checked to be nonempty
                        //println(s"-->> $g -- $t1")
                        (t.transform((g >> 1) + (0 -> (t1 >> 1)), i >> 1, Some(t2))).map { (te, t3) =>
                            (Abs(te, r1), Pro(t1, t3, r2))
                        }
                    case Some(other) => Result.fail(TypeError.Unexpected("product type"))
                    case _ => Result.fail(TypeError.Unexpected("product type"))
                // typecheck t1 and use the result to typecheck t2
                case App(t1, t2, r1) =>
                    t1.transform(g, i, None).flatMap { (te, ty) =>
                        te.searchAll(ty, i).flatMap {
                            case (abs, Pro(u1, u2, r2)) =>
                                t2.transform(g, i, Some(u1)).flatMap { (t2te, t2ty) =>
                                    App(abs, t2te, r1).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i)
                                }
                            case (_, _) => Result.fail(TypeError.Unexpected("product type"))
                        }
                    }
                // parameter and result should be a type
                case Pro(t1, t2, r1) =>
                    t1.transform(g, i, Some(Typ(r1))).flatMap { (u1, _) =>
                        t2.transform((g >> 1) + (0 -> (t1 >> 1)), i >> 1, Some(Typ(r1))).flatMap { (u2, _) =>
                            Pro(u1, u2, r1).checkWithSearch(Typ(r1), shape, i)
                        }
                    }
                // search for implicit to match shape
                case Phi(_) => shape match
                    case Some(value) => 
                        //println(s"CONTEXT $i SHAPE $value")
                        // throw new Exception()
                        search(value, i, i).map(Result.Success(_, value)).getOrElse(Result.fail(TypeError.NoImplicitFound(shape)))
                    case None => Result.fail(TypeError.NoImplicitFound(shape))
                // parameter and result should be a type
                case Imp(t1, t2, r) =>
                    t1.transform(g, i, Some(Typ(t1.r))).flatMap { (u1, _) =>
                        t2.transform(g >> 1, (i >> 1) + (t1 -> Var(0, None, this.r)), Some(Typ(r))).flatMap { (u2, _) =>
                            Imp(u1, u2, r).checkWithSearch(Typ(this.r), shape, i)
                        }
                    }
                // check te against ty and then verify the shape
                // case As(te, ty) => te.transform(g, i, Some(ty)).flatMap { (ue, uy) =>
                //     ue.checkWithSearch(uy, shape.orElse(Some(ty)), i)
                // }
                case As(te, ty, r) => te.transform(g, i, Some(ty)).flatMap { (ue, uy) =>
                    As(ue, ty, r).checkWithSearch(uy, shape.orElse(Some(ty)), i)
                }

                case Module(fields, _) => shape match
                    case None => checkModule(fields, Nil, Nil, g, i)
                    case Some(Interface(types, _)) => checkModuleWithInterface(fields.zip(types), Nil, Nil, g, i)
                    case Some(_) => Result.fail(TypeError.Unexpected("interface"))

                case Interface(fields, _) => checkInterface(fields, Nil, g, i).flatMap { (te, ty) =>
                    te.checkWithSearch(ty, shape, i)
                }

                case Get(t, field, r) => t.transform(g, i, None).flatMap { (te, ty) =>
                    te.searchAll(ty, i).flatMap {
                        case (te, Interface(fields, _)) => 
                            fields.find(_.name == field) match
                                case Some(IntElem(name, typ, mode)) =>
                                    //println(s"$typ -- $shape [${g.mkString(", ")}]")
                                    Get(te, field, r).checkWithSearch(typ.replace(0, te), shape, i) // should `te` be shifted?
                                case _ => Result.fail(TypeError.NoField(t, field))
                        case (te, ty) => 
                            println(s"found type $ty")
                            Result.fail(TypeError.NoField(t, field))
                    }
                }

                case Nat(value, r) => if value >= 0 then Result.Success(Nat(value, r), NatType(r)) else Result.fail(TypeError.Message("natural numbers are >= 0"))
                case NatType(r) => Result.Success(NatType(r), Typ(r))
                case Succ(t, r) => t.transform(g, i, Some(NatType(r))).flatMap((te, ty) => Succ(te, r).checkWithSearch(NatType(r), shape, i))
                case Debug(t, r) => t.transform(g, i, Some(NatType(r))).flatMap((te, ty) => Debug(te, r).checkWithSearch(NatType(r), shape, i))
                case Match(t, onZero, onSucc, r) => t.transform(g, i, Some(NatType(r))).flatMap { (te, ty) =>
                    onZero.transform(g, i, shape).flatMap { (z, zt) => 
                        onSucc.transform((g >> 1) + (0 -> NatType(r)), i, shape.map(_ >> 1)).flatMap { (s, st) =>
                            if (zt >> 1) === st then Result.Success(Match(te, z, s, r), zt) else Result.fail(TypeError.Message(s"the branches have different types: $zt and $st"))
                        }
                    }
                }

    /** helper of [[transform]] for checking modules with expected interface */
    def checkModuleWithInterface(fields: List[(ModElem, IntElem)], module: List[ModElem], interface: List[IntElem], g: G, i: I): Result[TypeError, (Module, Interface)] = fields match
        case Nil => Result.Success(Module(module.reverse, this.r), Interface(interface.reverse, this.r))
        case (ModElem(name, term, mode), IntElem(name1, typ, mode1)) :: next if name == name1 && mode == mode1 => 
            val expContext = (g >> 1) + (0 -> Interface(interface ++ fields.map(_(1)), this.r))
            val impContext = (i >> 1) ++ interface.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None, this.r), name, this.r)) }
            val taggedTerm = term.tag(0, Module(module, this.r) >> 1)
            val taggedType = typ.tag(0, Module(module, this.r) >> 1)
            taggedTerm.transform(expContext, impContext, Some(taggedType)).flatMap { (te, ty) =>
                checkModuleWithInterface(next, ModElem(name, te, mode) :: module, IntElem(name, typ, mode) :: interface, g, i)
            }
        case (ModElem(_, _, teMode), IntElem(_, _, tyMode)) :: _ => Result.fail(TypeError.Message(s"found $teMode for $tyMode"))
    
    /** helper of [[transform]] for checking modules without expected interface */
    def checkModule(fields: List[ModElem], module: List[ModElem], interface: List[IntElem], g: G, i: I): Result[TypeError, (Module, Interface)] = fields match
        case Nil => Result.Success(Module(module.reverse, this.r), Interface(interface.reverse, this.r))
        case ModElem(name, term, mode) :: next => 
            val expContext = (g >> 1) + (0 -> Interface(interface, this.r))
            val impContext = (i >> 1) ++ interface.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None, this.r), name, this.r)) }
            val taggedTerm = term.tag(0, Module(module, this.r) >> 1)
            taggedTerm.transform(expContext, impContext, None).flatMap { (te, ty) =>
                checkModule(next, ModElem(name, te, mode) :: module, IntElem(name, ty, mode) :: interface, g, i)
            }

    /** helper of [[transform]] for checking interfaces */
    def checkInterface(fields: List[IntElem], checked: List[IntElem], g: G, i: I): Result[TypeError, (Interface, Term.Typ)] = fields match
        case Nil => Result.Success(Interface(checked.reverse, this.r), Typ(this.r))
        case IntElem(name, typ, mode) :: next =>
            val expContext = (g >> 1) + (0 -> Interface(fields ++ checked, this.r))
            val impContext =  (i >> 1) ++ checked.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None, this.r), name, this.r)) }
            typ.transform(expContext, impContext, Some(Typ(this.r))).flatMap { (ty, _) => 
                checkInterface(next, IntElem(name, ty, mode) :: checked, g, i)
            }

    /** alias for [[transform transform(Map.empty, Map.empty, None)]] */
    def typeCheck = transform(Map.empty, Map.empty, None)

    /** alias for [[transform transform(Map.empty, Map.empty, Some(ty))]] */
    def typeCheck(ty: Term) = transform(Map.empty, Map.empty, Some(ty))

}