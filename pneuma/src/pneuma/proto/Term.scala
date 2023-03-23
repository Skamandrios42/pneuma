package pneuma.proto

import scala.annotation.targetName
import Term.{IntElem, ModElem, Mode}
import general.Result

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
enum Term {
    case Var(x: Int, tagged: Option[Term])
    case Abs(t: Term)
    case App(t1: Term, t2: Term)
    case Typ
    case Phi
    case Pro(t1: Term, t2: Term)
    case Imp(t1: Term, t2: Term)
    case Module(fields: List[ModElem])
    case Interface(fields: List[IntElem])
    case Get(t: Term, field: String)
    case As(te: Term, ty: Term)
    case NatType
    case Nat(value: Int)
    case Succ(t: Term)
    case Debug(t: Term)
    case Match(t: Term, onZero: Term, onSucc: Term)

    /** generates a simple string representation of the program */
    override def toString: String = this match
        case Var(x, Some(t)) => s"$x[$t]"
        case Var(x, None) => s"$x"
        case Abs(t) => s"(λ.$t)"
        case App(t1, t2) => s"($t1 $t2)"
        case Typ => "*"
        case Phi => "?"
        case Pro(t1, t2) => s"(π$t1.$t2)"
        case Imp(t1, t2) => s"(?$t1.$t2)"
        case Module(fields) => s"{ ${fields.mkString(", ")} }"
        case Interface(fields) => s"{ ${fields.mkString(", ")} }"
        case Get(t, field) => s"($t.$field)"
        case As(te, ty) => s"($te : $ty)"
        case NatType => "Nat"
        case Nat(value) => s"n$value"
        case Succ(t) => s"(S $t)"
        case Debug(t) => s"(debug $t)"
        case Match(t, onZero, onSucc) => s"($t match Z => $onZero, S => $onSucc )"

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
        case Var(x, Some(t)) => if x >= cutoff then Var(x + amount, Some(t.shift(amount, cutoff))) else Var(x, Some(t.shift(amount, cutoff)))
        case Var(x, None) => if x >= cutoff then Var(x + amount, None) else Var(x, None)
        case Abs(t) => Abs(t.shift(amount, cutoff + 1))
        case App(t1, t2) => App(t1.shift(amount, cutoff), t2.shift(amount, cutoff))
        case Typ => Typ
        case Phi => Phi
        case Pro(t1, t2) => Pro(t1.shift(amount, cutoff), t2.shift(amount, cutoff + 1))
        case Imp(t1, t2) => Imp(t1.shift(amount, cutoff), t2.shift(amount, cutoff))
        case Module(fields) => Module(fields.mapValues(_.shift(amount, cutoff + 1)))
        case Interface(fields) => Interface(fields.mapValues(_.shift(amount, cutoff + 1)))
        case Get(t, field) => Get(t.shift(amount, cutoff), field)
        case As(te, ty) => Term.As(te.shift(amount, cutoff), ty.shift(amount, cutoff))
        case NatType => NatType
        case Nat(value) => Nat(value)
        case Succ(t) => Succ(t.shift(amount, cutoff))
        case Debug(t) => Debug(t.shift(amount, cutoff))
        case Match(t, onZero, onSucc) => Match(t.shift(amount, cutoff), onZero.shift(amount, cutoff), onSucc.shift(amount, cutoff + 1))

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
        case Var(x, Some(e)) => if x == y then t else Var(x, Some(e.replace(y, t)))
        case Var(x, None) => if x == y then t else Var(x, None)
        case Abs(t1) => Abs(t1.replace(y + 1, t >> 1))
        case App(t1, t2) => App(t1.replace(y, t), t2.replace(y, t))
        case Typ => Typ
        case Phi => Phi
        case Pro(t1, t2) => Pro(t1.replace(y, t), t2.replace(y + 1, t >> 1))
        case Imp(t1, t2) => Imp(t1.replace(y, t), t2.replace(y + 1, t >> 1))
        case Module(fields) => Module(fields.mapValues(_.replace(y + 1, t >> 1)))
        case Interface(fields) => Interface(fields.mapValues(_.replace(y + 1, t >> 1)))
        case Get(t1, field) => Get(t1.replace(y, t), field)
        case As(te, ty) => Term.As(te.replace(y, t), ty.replace(y, t))
        case NatType => NatType
        case Nat(value) => Nat(value)
        case Succ(e) => Succ(e.replace(y, t))
        case Debug(e) => Debug(e.replace(y, t))
        case Match(e, onZero, onSucc) => Match(e.replace(y, t), onZero.replace(y, t), onSucc.replace(y + 1, t >> 1))

    /** similiar to [[replace]], but instead of replacing, `t` is added as in the `tagged` field of the variable
      * @param y the variable to be tagged
      * @param t the term to be inserted
      * @return `this` with tagged variables
      */
    def tag(y: Int, t: Term): Term = this match
        case Var(x, Some(e)) => 
            //if x == y then println(s"TAGGED $x WITH $t")
            if x == y then Var(x, Some(t)) else Var(x, Some(e.tag(y, t)))
        case Var(x, None) => 
            //if x == y then println(s"TAGGED $x WITH $t")
            if x == y then Var(x, Some(t)) else Var(x, None)
        case Abs(t1) => Abs(t1.tag(y + 1, t >> 1))
        case App(t1, t2) => App(t1.tag(y, t), t2.tag(y, t))
        case Typ => Typ
        case Phi => Phi
        case Pro(t1, t2) => Pro(t1.tag(y, t), t2.tag(y + 1, t >> 1))
        case Imp(t1, t2) => Imp(t1.tag(y, t), t2.tag(y + 1, t >> 1))
        case Module(fields) => Module(fields.mapValues(_.tag(y + 1, t >> 1)))
        case Interface(fields) => Interface(fields.mapValues(_.tag(y + 1, t >> 1)))
        case Get(t1, field) => Get(t1.tag(y, t), field)
        case As(te, ty) => Term.As(te.tag(y, t), ty.tag(y, t))
        case NatType => NatType
        case Nat(value) => Nat(value)
        case Succ(e) => Succ(e.tag(y, t))
        case Debug(e) => Debug(e.tag(y, t))
        case Match(e, onZero, onSucc) => Match(e.tag(y, t), onZero.tag(y, t), onSucc.tag(y + 1, t >> 1))

    /** removes all tags from the variables */
    def untag: Term = this match
        case Var(x, _) => Var(x, None)
        case Abs(t1) => Abs(t1.untag)
        case App(t1, t2) => App(t1.untag, t2.untag)
        case Typ => Typ
        case Phi => Phi
        case Pro(t1, t2) => Pro(t1.untag, t2.untag)
        case Imp(t1, t2) => Imp(t1.untag, t2.untag)
        case Module(fields) => Module(fields.mapValues(_.untag))
        case Interface(fields) => Interface(fields.mapValues(_.untag))
        case Get(t1, field) => Get(t1.untag, field)
        case As(te, ty) => Term.As(te.untag, ty.untag)
        case NatType => NatType
        case Nat(value) => Nat(value)
        case Succ(e) => Succ(e.untag)
        case Debug(e) => Debug(e.untag)
        case Match(t, onZero, onSucc) => Match(t.untag, onZero.untag, onSucc.untag)
        

    /** evaluates `this` using the _call-by-value_ strategy */
    def eval: Term = this match
        case Term.App(t1, t2) => (t1.eval, t2.eval) match
            case (Term.Abs(body), arg) => (body.replace(0, arg >> 1) << 1).eval
            case (abs, arg) => Term.App(abs, arg)
        case Term.Get(t, field) => t.eval match
            case mod @ Module(fields) => 
                fields.find(_.name == field) match
                    case Some(ModElem(_, value, _)) => (value.replace(0, mod >> 1) << 1).eval
                    case _ => Term.Get(mod, field)
            case e => Term.Get(e, field)
        case Term.As(te, ty) => te
        case Var(x, Some(t)) => t
        case Succ(e) => e.eval match
            case Nat(value) => Nat(value + 1)
            case e => e
        case Debug(e) => e.eval match
            case Nat(value) =>
                println(s"debug $value") 
                Nat(value)
            case e => e
        case Match(e, onZero, onSucc) => e.eval match
            case Nat(0) => onZero.eval
            case Nat(other) => App(Abs(onSucc), Nat(other - 1)).eval
            case other => Match(other, onZero, onSucc)
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
                case (Var(x, _), Var(y, _)) => x == y
                case (Abs(t), Abs(e)) => t.equivalence(e, updatedRelation >> 1)
                case (App(t1, t2), App(e1, e2)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (Typ, Typ) => true
                case (Phi, Phi) => throw new Exception("equivalence should only be called after implicit resolution")
                case (Pro(t1, t2), Pro(e1, e2)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation >> 1)
                case (Imp(t1, t2), Imp(e1, e2)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation >> 1)
                case (Module(ts), Module(es)) => (ts zip es).forall {
                    case (ModElem(s1, t1, m1), ModElem(s2, t2, m2)) => s1 == s2 && m1 == m2 && t1.equivalence(t2, updatedRelation >> 1)
                }
                case (Interface(ts), Interface(es)) => (ts zip es).forall {
                    case (IntElem(s1, t1, m1), IntElem(s2, t2, m2)) => s1 == s2 && m1 == m2 && t1.equivalence(t2, updatedRelation >> 1)
                }
                case (Get(t, f1), Get(e, f2)) => f1 == f2 && t.equivalence(e, updatedRelation)
                case (As(t1, t2), As(e1, e2)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case (NatType, NatType) => true
                case (Nat(v1), Nat(v2)) => v1 == v2
                case (Succ(t1), Succ(t2)) => t1.equivalence(t2, updatedRelation)
                case (Debug(t1), Debug(t2)) => t1.equivalence(t2, updatedRelation)
                case (Match(t1, z1, s1), Match(t2, z2, s2)) => t1.equivalence(t2, updatedRelation) && z1.equivalence(z2, updatedRelation) && s1.equivalence(s2, updatedRelation)
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
        case Some((Imp(t1, t2), i)) if t2 === (typ >> 1) => 
            //println(s"found implicit function $t1 -> $t2")
            search(t1, all, all).map { arg => App(i, arg) }
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
            else Result.fail(TypeError.Mismatch(shape.get, ty))
        res.orElse { (ty, shape) match
            case (Imp(t1, t2), shape) =>
                search(t1, i, i) match
                    case None => Result.fail(TypeError.Mismatch(shape.get, ty))
                    case Some(arg) => checkWithSearch(t2, shape, i).map { (te, ty) => (App(te, arg), ty) }
            case _ => Result.fail(TypeError.Mismatch(shape.get, ty))
        }

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
        if this == Typ && shape.contains(Typ) then Result.Success(Typ, Typ)
        else shape.map(_.transform(g, i, Some(Typ)).map(_(0).eval)) match
            // add implicit in context // SHOULD t1 be checked to be nonempty ?
            case Some(Result.Success(Imp(t1, t2))) =>
                t1.transform(g, i, Some(Typ)).flatMap((t1te, _) => // SOMETHING IS REALLY WRONG HERE
                    (this >> 1).transform(g >> 1, (i >> 1) + ((t1te >> 1) -> Var(0, None)), Some(t2)).map((te, ty) => (Abs(te), Imp(t1te, ty)))
                )
            case Some(Result.Failure(value)) => Result.Failure(value)
            // otherwise match on this and apply typechecking rules
            case e          => 
                val shape = e.collect {
                    case Result.Success(x) => x
                }
                this match
                // checks if Typ matches shape
                case Typ => Typ.checkWithSearch(Typ, shape, i)
                // get type from context
                case Var(x, tag) => g get x match
                    // checks if variable matches shape
                    case Some(value) => Var(x, tag).checkWithSearch(value, shape, i)
                    // report undefined variable type
                    case None => Result.fail(TypeError.Undefined(x.toString))
                // use shape to generate type for abstraction
                case Abs(t) => shape match       // Done WILL SHAPE BE NORMAL-FORM? I think not e. g. because of t2  (... evaluate with implicits inserted !!)
                    case Some(Pro(t1, t2)) =>    // Done SHOULD t1 be checked to be nonempty
                        //println(s"-->> $g -- $t1")
                        (t.transform((g >> 1) + (0 -> (t1 >> 1)), i >> 1, Some(t2))).map { (te, t3) =>
                            (Abs(te), Pro(t1, t3))
                        }
                    case Some(other) => Result.fail(TypeError.Unexpected("product type"))
                    case _ => Result.fail(TypeError.Unexpected("product type"))
                // typecheck t1 and use the result to typecheck t2
                case App(t1, t2) =>
                    t1.transform(g, i, None).flatMap {
                        case (abs, Pro(u1, u2)) =>
                            t2.transform(g, i, Some(u1)).flatMap { (t2te, t2ty) =>
                                App(abs, t2te).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i)
                            }
                        case (_, _) => Result.fail(TypeError.Unexpected("product type"))
                    }
                // parameter and result should be a type
                case Pro(t1, t2) =>
                    t1.transform(g, i, Some(Typ)).flatMap { (u1, _) =>
                        t2.transform((g >> 1) + (0 -> (t1 >> 1)), i >> 1, Some(Typ)).flatMap { (u2, _) =>
                            Pro(u1, u2).checkWithSearch(Typ, shape, i)
                        }
                    }
                // search for implicit to match shape
                case Phi => shape match
                    case Some(value) => 
                        //println(s"CONTEXT $i SHAPE $value")
                        // throw new Exception()
                        search(value, i, i).map(Result.Success(_, value)).getOrElse(Result.fail(TypeError.NoImplicitFound(shape)))
                    case None => Result.fail(TypeError.NoImplicitFound(shape))
                // parameter and result should be a type
                case Imp(t1, t2) =>
                    t1.transform(g, i, Some(Typ)).flatMap { (u1, _) =>
                        t2.transform(g >> 1, (i >> 1) + (t1 -> Var(0, None)), Some(Typ)).flatMap { (u2, _) =>
                            Imp(u1, u2).checkWithSearch(Typ, shape, i)
                        }
                    }
                // check te against ty and then verify the shape
                // case As(te, ty) => te.transform(g, i, Some(ty)).flatMap { (ue, uy) =>
                //     ue.checkWithSearch(uy, shape.orElse(Some(ty)), i)
                // }
                case As(te, ty) => te.transform(g, i, Some(ty)).flatMap { (ue, uy) =>
                    As(ue, ty).checkWithSearch(uy, shape.orElse(Some(ty)), i)
                }

                case Module(fields) => shape match
                    case None => checkModule(fields, Nil, Nil, g, i)
                    case Some(Interface(types)) => checkModuleWithInterface(fields.zip(types), Nil, Nil, g, i)
                    case Some(_) => Result.fail(TypeError.Unexpected("interface"))

                case Interface(fields) => checkInterface(fields, Nil, g, i).flatMap { (te, ty) =>
                    te.checkWithSearch(ty, shape, i)
                }

                case Get(t, field) => t.transform(g, i, None).flatMap { 
                    case (te, Interface(fields)) => 
                        fields.find(_.name == field) match
                            case Some(IntElem(name, typ, mode)) => 
                                //println(s"$typ -- $shape [${g.mkString(", ")}]")
                                Get(te, field).checkWithSearch(typ.replace(0, te), shape, i) // should `te` be shifted?
                            case _ => Result.fail(TypeError.NoField(t, field))
                    case _ => Result.fail(TypeError.NoField(t, field))
                }

                case Nat(value) => if value >= 0 then Result.Success(Nat(value), NatType) else Result.fail(TypeError.Message("natural numbers are >= 0"))
                case NatType => Result.Success(NatType, Typ)
                case Succ(t) => t.transform(g, i, Some(NatType)).flatMap((te, ty) => Succ(te).checkWithSearch(NatType, shape, i))
                case Debug(t) => t.transform(g, i, Some(NatType)).flatMap((te, ty) => Debug(te).checkWithSearch(NatType, shape, i))
                case Match(t, onZero, onSucc) => t.transform(g, i, Some(NatType)).flatMap { (te, ty) =>
                    onZero.transform(g, i, shape).flatMap { (z, zt) => 
                        onSucc.transform((g >> 1) + (0 -> NatType), i, shape.map(_ >> 1)).flatMap { (s, st) =>
                            if (zt >> 1) === st then Result.Success(Match(te, z, s), zt) else Result.fail(TypeError.Message(s"the branches have different types: $zt and $st"))
                        }
                    }
                }

    /** helper of [[transform]] for checking modules with expected interface */
    def checkModuleWithInterface(fields: List[(ModElem, IntElem)], module: List[ModElem], interface: List[IntElem], g: G, i: I): Result[TypeError, (Module, Interface)] = fields match
        case Nil => Result.Success(Module(module.reverse), Interface(interface.reverse))
        case (ModElem(name, term, mode), IntElem(name1, typ, mode1)) :: next if name == name1 && mode == mode1 => 
            val expContext = (g >> 1) + (0 -> Interface(interface ++ fields.map(_(1))))
            val impContext = (i >> 1) ++ interface.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None), name)) }
            val taggedTerm = term.tag(0, Module(module) >> 1)
            val taggedType = typ.tag(0, Module(module) >> 1)
            taggedTerm.transform(expContext, impContext, Some(taggedType)).flatMap { (te, ty) =>
                checkModuleWithInterface(next, ModElem(name, te, mode) :: module, IntElem(name, typ, mode) :: interface, g, i)
            }
        case (ModElem(_, _, teMode), IntElem(_, _, tyMode)) :: _ => Result.fail(TypeError.Message(s"found $teMode for $tyMode"))
    
    /** helper of [[transform]] for checking modules without expected interface */
    def checkModule(fields: List[ModElem], module: List[ModElem], interface: List[IntElem], g: G, i: I): Result[TypeError, (Module, Interface)] = fields match
        case Nil => Result.Success(Module(module.reverse), Interface(interface.reverse))
        case ModElem(name, term, mode) :: next => 
            val expContext = (g >> 1) + (0 -> Interface(interface))
            val impContext = (i >> 1) ++ interface.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None), name)) }
            val taggedTerm = term.tag(0, Module(module) >> 1)
            taggedTerm.transform(expContext, impContext, None).flatMap { (te, ty) =>
                checkModule(next, ModElem(name, te, mode) :: module, IntElem(name, ty, mode) :: interface, g, i)
            }

    /** helper of [[transform]] for checking interfaces */
    def checkInterface(fields: List[IntElem], checked: List[IntElem], g: G, i: I): Result[TypeError, (Interface, Term.Typ.type)] = fields match
        case Nil => Result.Success(Interface(checked.reverse), Typ)
        case IntElem(name, typ, mode) :: next =>
            val expContext = (g >> 1) + (0 -> Interface(fields ++ checked))
            val impContext =  (i >> 1) ++ checked.collect { case IntElem(name, typ, Mode.Imp) => (typ, Get(Var(0, None), name)) }
            typ.transform(expContext, impContext, Some(Typ)).flatMap { (ty, _) => 
                checkInterface(next, IntElem(name, ty, mode) :: checked, g, i)
            }

    /** alias for [[transform transform(Map.empty, Map.empty, None)]] */
    def typeCheck = transform(Map.empty, Map.empty, None)
    
    /** alias for [[transform transform(Map.empty, Map.empty, Some(ty))]] */
    def typeCheck(ty: Term) = transform(Map.empty, Map.empty, Some(ty))

}