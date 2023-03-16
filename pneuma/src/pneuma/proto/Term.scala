package pneuma.proto

import scala.annotation.targetName

/** class to generate new names of the format 'base0000'
  * @note not thread-safe, uses mutable state, should probably be removed in later versions
  * @param base the root string of a generated name
  */
class Namer(val base: String) {
    private var index = 0
    def next() =
        val name = f"$base$index%04d"
        index += 1
        name
}

/** a definition in a module; either a [[ModElem.Named named]] or an [[ModElem.Imp implicit]] definition */
enum ModElem {
    case Named(name: String, term: Term)
    case Imp(typ: Term, term: Term)

    /** @return the name, if this is [[ModElem.Named]] */
    def ident = this match
        case Named(name, term) => Some(name)
        case Imp(typ, term) => None

    override def toString(): String = this match
        case Named(name, term) => s"$name = $term"
        case Imp(typ, term) => s"? : $typ = $term"
}

/** a definitions type in an interface; either for [[IntElem.Named named]] or for [[IntElem.Imp implicit]] definitions */
enum IntElem {
    case Named(name: String, typ: Term)
    case Imp(name: String, typ: Term)

    /** @return the name, if this is [[IntElem.Named]] */
    def ident = this match
        case Named(name, term) => Some(name)
        case Imp(name, typ) => None

    override def toString(): String = this match
        case Named(name, term) => s"$name : $term"
        case Imp(name, typ) => s"$name ?: $typ"
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

    /** generates a simple string representation of the program */
    override def toString: String = this match
        case Term.Var(x, Some(t)) => s"$x[$t]"
        case Term.Var(x, None) => s"$x"
        case Term.Abs(t) => s"(λ.$t)"
        case Term.App(t1, t2) => s"($t1 $t2)"
        case Term.Typ => "*"
        case Term.Phi => "?"
        case Term.Pro(t1, t2) => s"(π$t1.$t2)"
        case Term.Imp(t1, t2) => s"(?$t1.$t2)"
        case Term.Module(fields) => s"{ ${fields.mkString(", ")} }"
        case Term.Interface(fields) => s"{ ${fields.mkString(", ")} }"
        case Term.Get(t, field) => s"($t.$field)"
        case Term.As(te, ty) => s"($te : $ty)"

    
    extension [A](self: List[ModElem]) {
        /** maps f over every occurrence of [[Term]]
          * @param f a function from Term to Term
          * @return the mapped ModElem
          */
        @targetName("mapValuesModElem")
        def mapValues(f: Term => Term) = self.map {
            case ModElem.Named(name, term) => ModElem.Named(name, f(term))
            case ModElem.Imp(typ, term) => ModElem.Imp(f(typ), f(term))
        }
    }

    extension [A](self: List[IntElem]) {
        /** maps f over every occurrence of [[Term]]
          * @param f a function from Term to Term
          * @return the mapped IntElem
          */
        @targetName("mapValuesIntElem")
        def mapValues(f: Term => Term) = self.map {
            case IntElem.Named(name, term) => IntElem.Named(name, f(term))
            case IntElem.Imp(name, typ) => IntElem.Imp(name, f(typ))
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
        case Pro(t1, t2) => Pro(t1, t2.shift(amount, cutoff + 1))
        case Imp(t1, t2) => Imp(t1, t2.shift(amount, cutoff + 1))
        case Module(fields) => Module(fields.mapValues(_.shift(amount, cutoff + 1)))
        case Interface(fields) => Interface(fields.mapValues(_.shift(amount, cutoff + 1)))
        case Get(t, field) => Get(t.shift(amount, cutoff), field)
        case Term.As(te, ty) => Term.As(te.shift(amount, cutoff), ty.shift(amount, cutoff))

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
        case Term.As(te, ty) => Term.As(te.replace(y, t), ty.replace(y, t))

    /** similiar to [[replace]], but instead of replacing, `t` is added as in the `tagged` field of the variable
      * @param y the variable to be tagged
      * @param t the term to be inserted
      * @return `this` with tagged variables
      */
    def tag(y: Int, t: Term): Term = this match
        case Var(x, Some(e)) => 
            if x == y then
                println("Warning! Retagged variable!")
                Var(x, Some(t))
            else Var(x, Some(e.tag(y, t)))
        case Var(x, None) => if x == y then Var(x, Some(t)) else Var(x, None)
        case Abs(t1) => Abs(t1.tag(y + 1, t >> 1))
        case App(t1, t2) => App(t1.tag(y, t), t2.tag(y, t))
        case Typ => Typ
        case Phi => Phi
        case Pro(t1, t2) => Pro(t1.tag(y, t), t2.tag(y + 1, t >> 1))
        case Imp(t1, t2) => Imp(t1.tag(y, t), t2.tag(y + 1, t >> 1))
        case Module(fields) => Module(fields.mapValues(_.tag(y + 1, t >> 1)))
        case Interface(fields) => Interface(fields.mapValues(_.tag(y + 1, t >> 1)))
        case Get(t1, field) => Get(t1.tag(y, t), field)
        case Term.As(te, ty) => Term.As(te.tag(y, t), ty.tag(y, t))

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
        case Term.As(te, ty) => Term.As(te.untag, ty.untag)

    /** evaluates `this` using the _call-by-value_ strategy */
    def eval: Term = this match
        case Term.App(t1, t2) => (t1.eval, t2.eval) match
            case (Term.Abs(body), arg) => (body.replace(0, arg >> 1) << 1).eval
            case (abs, arg) => Term.App(abs, arg)
        case Term.Get(t, field) => t.eval match
            case mod @ Module(fields) => 
                fields.find(_.ident.exists(_ == field)) match
                    case Some(ModElem.Named(_, value)) => (value.replace(0, mod >> 1) << 1).eval
                    case _ => Term.Get(mod, field)
            case e => Term.Get(e, field)
        case Term.As(te, ty) => te
        case Var(x, Some(t)) => t
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
                case (Phi, Phi) => ???
                case (Pro(t1, t2), Pro(e1, e2)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation >> 1)
                case (Imp(t1, t2), Imp(e1, e2)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation >> 1)
                case (Module(ts), Module(es)) => (ts zip es).forall {
                    case (ModElem.Named(s1, t), ModElem.Named(s2, e)) => s1 == s2 && t.equivalence(e, updatedRelation >> 1)
                    case (ModElem.Imp(t1, t2), ModElem.Imp(e1, e2)) => t1.equivalence(e1, updatedRelation >> 1) && t2.equivalence(e2, updatedRelation >> 1)
                    case _ => false
                }
                case (Interface(ts), Interface(es)) => (ts zip es).forall {
                    case (IntElem.Named(s1, t), IntElem.Named(s2, e)) => s1 == s2 && t.equivalence(e, updatedRelation >> 1)
                    case (IntElem.Imp(s1, t), IntElem.Imp(s2, e)) => s1 == s2 && t.equivalence(e, updatedRelation >> 1)
                    case _ => false
                }
                case (Get(t, f1), Get(e, f2)) => f1 == f2 && t.equivalence(e, updatedRelation)
                case (As(t1, t2), As(e1, e2)) => t1.equivalence(e1, updatedRelation) && t2.equivalence(e2, updatedRelation)
                case _ => false

    /** checks, if this (seen as a type) conforms to the expected shape using [[===]]
      * @param shape optional type
      * @return `this === shape.get`, if shape is defined or else true
      */
    def matches(shape: Option[Term]): Boolean = 
        println(s"DEBUG: $this should match $shape")
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
    def search(typ: Term, i: I, all: I): Option[Term] = i.headOption match
        case Some(imp, index) if imp === typ => Some(index)
        case Some((Imp(t1, t2), i)) => search(t1, all, all).map { arg => App(i, arg) }
        case Some(_) => search(typ, i.tail, all)
        case None => None

    /** Checks that `ty` conforms to `shape` up to implicit resolution.
      * Uses [[matches]] and [[search]] to synthesize a term such that `ty` matches `shape`. 
      * If `ty` does not match `shape`, but is an implicit function type, then the implicit scope `i` is accessed.
      * @param ty type of `this`
      * @param shape expected type of `this`
      * @param i the implicit scope
      * @return type-checking result
      */
    def checkWithSearch(ty: Term, shape: Option[Term], i: I): Either[String, (Term, Term)] =
        val res = if ty matches shape then Right(this, shape.getOrElse(ty)) else Left(s"$ty does not match $shape")
        res.orElse { (ty, shape) match
            case (Imp(t1, t2), shape) =>
                search(t1, i, i) match
                    case None => Left(s"$ty does not match $shape")
                    case Some(arg) => checkWithSearch(t2, shape, i).map { (te, ty) => (App(te, arg), ty) }
            case _ => Left(s"$ty does not match $shape")
        }

    /** Generate the type of `this` and an expected `shape` of the type, while simultaneously resolving implicits.
      * @param g explicit scope
      * @param i implicit scope
      * @param shape expected shape of the type
      * @return either term and type with resolved implicits or an error message
      */
    def transform(g: G, i: I, shape: Option[Term]): Either[String, (Term, Term)] =
        shape match
            case None => println(s"[DEBUG] $this")
            case Some(value) => println(s"[DEBUG] $this : $value")
        // typecheck and transform the shape
        if this == Typ && shape.contains(Typ) then Right(Typ, Typ)
        else shape.map(_.transform(g, i, Some(Typ)).map(_(0).eval)) match
            // add implicit in context // SHOULD t1 be checked to be nonempty ?
            case Some(Right(Imp(t1, t2))) =>
                t1.transform(g, i, Some(Typ)).flatMap((t1te, _) =>
                    (this >> 1).transform(g >> 1, i + (t1te -> Var(0, None)), Some(t2)).map((te, ty) => (Abs(te), Imp(t1, ty)))
                )
            // otherwise match on this and apply typechecking rules
            case _           => this match
                // checks if Typ matches shape
                case Typ => Typ.checkWithSearch(Typ, shape, i)
                // get type from context
                case Var(x, tag) => g get x match
                    // checks if variable matches shape
                    case Some(value) => Var(x, tag).checkWithSearch(value, shape, i)
                    // report undefined variable type
                    case None => Left("undefined variable")
                // use shape to generate type for abstraction
                case Abs(t) => shape match       // Done WILL SHAPE BE NORMAL-FORM? I think not e. g. because of t2  (... evaluate with implicits inserted !!)
                    case Some(Pro(t1, t2)) =>    // Done SHOULD t1 be checked to be nonempty
                        (t.transform((g >> 1) + (0 -> t1), i >> 1, Some(t2))).map { (te, t3) =>
                            (Abs(te), Pro(t1, t3))
                        }
                    case _ => Left("type neccessary to type check abstraction")
                // typecheck t1 and use the result to typecheck t2
                case App(t1, t2) =>
                    t1.transform(g, i, None).flatMap {
                        case (abs, Pro(u1, u2)) =>
                            t2.transform(g, i, Some(u1)).flatMap { (t2te, t2ty) =>
                                App(abs, t2te).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i)
                            }
                        case (_, _) => Left(s"$t1 should have a product type")
                    }
                // parameter and result should be a type
                case Pro(t1, t2) =>
                    t1.transform(g, i, Some(Typ)).flatMap { (u1, _) =>
                        t2.transform((g >> 1) + (0 -> t1), i >> 1, Some(Typ)).flatMap { (u2, _) =>
                            Pro(u1, u2).checkWithSearch(Typ, shape, i)
                        }
                    }
                // search for implicit to match shape
                case Phi => shape match
                    case Some(value) => search(value, i, i).map(Right(_, value)).getOrElse(Left(s"no implicit found for $shape"))
                    case None => Left(s"no implicit found for $shape")
                // parameter and result should be a type
                case Imp(t1, t2) =>
                    t1.transform(g, i, Some(Typ)).flatMap { (u1, _) =>
                        t2.transform(g, i + (t1 -> Var(0, None)), Some(Typ)).flatMap { (u2, _) =>
                            Imp(u1, u2).checkWithSearch(Typ, shape, i)
                        }
                    }
                // check te against ty and then verify the shape
                case As(te, ty) => te.transform(g, i, Some(ty)).flatMap { (ue, uy) =>
                    ue.checkWithSearch(uy, shape, i)
                }

                //case mod: Module if m contains mod => Right(mod, m(mod))

                // TODO consider shape
                case Module(fields) => shape match
                    case None => checkModule(fields, Nil, Nil, g >> 1, i >> 1, Namer("imp$"))
                    case Some(Interface(types)) => checkModuleWithInterface(fields.zip(types), Nil, Nil, g, i, Namer("imp$"))
                    case Some(_) => Left("you're dumb")
                // TODO consider shape
                case Interface(fields) => checkInterface(fields, Nil, g >> 1, i >> 1).flatMap { (te, ty) =>
                    te.checkWithSearch(ty, shape, i)
                }

                case Get(t, field) => t.transform(g, i, None).flatMap { 
                    case (te, Interface(fields)) => 
                        fields.find(_.ident.exists(_ == field)) match
                            case Some(IntElem.Named(name, typ)) => 
                                Get(te, field).checkWithSearch(typ, shape, i)
                            case _ => Left("no '$field' field")
                    case _ => Left("no '$field' field")
                }

    /** helper of [[transform]] for checking modules with expected interface */
    def checkModuleWithInterface(fields: List[(ModElem, IntElem)], module: List[ModElem], interface: List[IntElem], g: G, i: I, namer: Namer): Either[String, (Module, Interface)] = fields match
        case Nil => Right(Module(module.reverse), Interface(interface.reverse))
        case (ModElem.Named(name, term), IntElem.Named(_name, typ)) :: next if name == _name => 
            val expContext = (g >> 1) + (0 -> Interface(interface ++ fields.map(_(1))))
            val impContext = (i >> 1) ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            val taggedTerm = term.tag(0, Module(module) >> 1)
            val taggedType = typ.tag(0, Module(module) >> 1)
            taggedTerm.transform(expContext, impContext, Some(taggedType)).flatMap { (te, ty) =>
                checkModuleWithInterface(next, ModElem.Named(name, te) :: module, IntElem.Named(name, ty) :: interface, g, i, namer)
            }
        case (ModElem.Imp(typ, term), IntElem.Imp(name, typ2)) :: next if typ === typ2 =>
            val expContext = (g >> 1) + (0 -> Interface(interface ++ fields.map(_(1))))
            val impContext = (i >> 1) ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            val taggedTerm = term.tag(0, Module(module) >> 1)
            typ.transform(expContext, impContext, Some(Typ)).flatMap { (u, _) => 
                val taggedType = u.tag(0, Module(module) >> 1)
                taggedTerm.transform((g >> 1) + (0 -> Interface(interface)), impContext, Some(taggedType)).flatMap { (te, ty) =>
                    checkModuleWithInterface(next, ModElem.Named(name, te) :: module, IntElem.Imp(name, ty) :: interface, g, i, namer)
                }
            }
        case _ => Left("Type not matching!")
    
    /** helper of [[transform]] for checking modules without expected interface */
    def checkModule(fields: List[ModElem], module: List[ModElem], interface: List[IntElem], g: G, i: I, namer: Namer): Either[String, (Module, Interface)] = fields match
        case Nil => Right(Module(module.reverse), Interface(interface.reverse))
        case ModElem.Named(name, term) :: next => 
            val expContext = (g >> 1) + (0 -> Interface(interface))
            val impContext = (i >> 1) ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            val taggedTerm = term.tag(0, Module(module) >> 1)
            taggedTerm.transform(expContext, impContext, None).flatMap { (te, ty) =>
                checkModule(next, ModElem.Named(name, te) :: module, IntElem.Named(name, ty) :: interface, g, i, namer)
            }
        case ModElem.Imp(typ, term) :: next =>
            val expContext = (g >> 1) + (0 -> Interface(interface))
            val impContext = (i >> 1) ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            val taggedTerm = term.tag(0, Module(module) >> 1)
            typ.transform((g >> 1) + (0 -> Interface(interface)), impContext, Some(Typ)).flatMap { (u, _) => 
                val taggedType = u.tag(0, Module(module) >> 1)
                taggedTerm.transform(expContext, impContext, Some(taggedType)).flatMap { (te, ty) =>
                    val name = namer.next()
                    checkModule(next, ModElem.Named(name, te) :: module, IntElem.Imp(name, ty) :: interface, g, i, namer)
                }
            }

    /** helper of [[transform]] for checking interfaces */
    def checkInterface(fields: List[IntElem], checked: List[IntElem], g: G, i: I): Either[String, (Interface, Term.Typ.type)] = fields match
        case Nil => Right(Interface(checked.reverse), Typ)
        case IntElem.Named(name, typ) :: next =>
            val expContext = (g >> 1) + (0 -> Interface(fields ++ checked))
            val impContext =  (i >> 1) ++ checked.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            typ.transform(expContext, impContext, Some(Typ)).flatMap { (ty, _) => 
                checkInterface(next, IntElem.Named(name, ty) :: checked, g, i)
            }
        case IntElem.Imp(name, typ) :: next =>
            val expContext = (g >> 1) + (0 -> Interface(fields ++ checked))
            val impContext =  (i >> 1) ++ checked.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            typ.transform(expContext, impContext, Some(Typ)).flatMap { (ty, _) => 
                checkInterface(next, IntElem.Imp(name, ty) :: checked, g, i)
            }

    /** alias for [[transform transform(Map.empty, Map.empty, None)]] */
    def typeCheck = transform(Map.empty, Map.empty, None)
    
    /** alias for [[transform transform(Map.empty, Map.empty, Some(ty))]] */
    def typeCheck(ty: Term) = transform(Map.empty, Map.empty, Some(ty))

}