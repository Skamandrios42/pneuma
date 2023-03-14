package pneuma.proto

import scala.annotation.targetName

class Namer(val base: String) {
    private var index = 0
    def next() =
        val name = f"$base$index%04d"
        index += 1
        name
}

enum ModElem {
    case Named(name: String, term: Term)
    case Imp(typ: Term, term: Term)
    override def toString(): String = this match
        case Named(name, term) => s"$name = $term"
        case Imp(typ, term) => s"φ : $typ = $term"
    def ident = this match
        case Named(name, term) => Some(name)
        case Imp(typ, term) => None
}
enum IntElem {
    case Named(name: String, typ: Term)
    case Imp(name: String, typ: Term)
    override def toString(): String = this match
        case Named(name, term) => s"$name : $term"
        case Imp(name, typ) => s"φ : $typ"
    def ident = this match
        case Named(name, term) => Some(name)
        case Imp(name, typ) => None
}

enum Term {
    // abstract syntax tree
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

    // toString implementation
    override def toString: String = this match
        case Term.Var(x, Some(t)) => s"$x[$t]"
        case Term.Var(x, None) => s"$x"
        case Term.Abs(t) => s"(λ.$t)"
        case Term.App(t1, t2) => s"($t1 $t2)"
        case Term.Typ => "*"
        case Term.Phi => "φ"
        case Term.Pro(t1, t2) => s"(π$t1.$t2)"
        case Term.Imp(t1, t2) => s"(φ$t1.$t2)"
        case Term.Module(fields) => s"{ ${fields.mkString(", ")} }"
        case Term.Interface(fields) => s"{ ${fields.mkString(", ")} }"
        case Term.Get(t, field) => s"($t.$field)"
        case Term.As(te, ty) => s"($te : $ty)"

    // module / interface manipulators
    extension [K, V](self: List[(K, V)]) {
        def mapValues[W](f: V => W): List[(K, W)] = self.map { case (key, value) => (key, f(value)) }
    }
    extension [A](self: List[(A, A)]) {
        def mapBoth[B](f: A => B): List[(B, B)] = self.map { case (key, value) => (f(key), f(value)) }
    }

    extension [A](self: List[ModElem]) {
        @targetName("mapValuesForModElem")
        def mapValues(f: Term => Term) = self map {
            case ModElem.Named(name, term) => ModElem.Named(name, f(term))
            case ModElem.Imp(typ, term) => ModElem.Imp(f(typ), f(term))
        }
    }
    extension [A](self: List[IntElem]) {
        @targetName("mapValuesForIntElem")
        def mapValues(f: Term => Term) = self map {
            case IntElem.Named(name, term) => IntElem.Named(name, f(term))
            case IntElem.Imp(name, typ) => IntElem.Imp(name, f(typ))
        }
    }

    // helpers for evaluation
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

    def >>(amount: Int) = shift(amount, 0)
    def <<(amount: Int) = shift(-amount, 0)

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


    // contexts and context shifters
    type G = Map[Int, Term]
    type M = Map[Term, Term]
    type P = Set[Term]
    type I = Map[Term, Term] // map von Typen zu Termen (Variablen-Namen oder Module Zugriffe)

    extension (self: G) {
        @targetName("shiftG")
        def >>(amount: Int): G = self.map { case (key, value) => (key + amount, value >> amount) }
    }

    extension (self: M) { // auch für I
        @targetName("shiftM")
        def >>(amount: Int): M = self.map { case (key, value) => (key >> amount, value >> amount) }
    }

    extension (self: P) {
        @targetName("shiftP")
        def >>(amount: Int): P = self.map { _ >> amount }
    }

    // equivalence relation

    def ===(that: Term): Boolean = this == that

    def <=<(that: Option[Term]): Boolean = that match
        case Some(value) => this === value
        case None => true

    // type checking

    def search(typ: Term, i: I, all: I): Option[Term] = i.headOption match
        case Some(imp, index) if imp === typ => Some(index)
        case Some((Imp(t1, t2), i)) => search(t1, all, all).map { arg => App(i, arg) }
        case Some(_) => search(typ, i.tail, all)
        case None => None

    def checkWithSearch(ty: Term, shape: Option[Term], i: I): Either[String, (Term, Term)] =
        val res = if ty <=< shape then Right(this, ty) else Left(s"$this does not match $shape")
        res.orElse { (ty, shape) match
            case (Imp(t1, t2), shape) =>
                search(t1, i, i) match
                    case None => Left(s"$this does not match $shape")
                    case Some(arg) => checkWithSearch(t2, shape, i).map { (te, ty) => (App(te, arg), ty) }
            case _ => Left(s"$this does not match $shape")
        }

    /** generates a term and its type from `this` and an expected `shape` of the type
      *
      * @param g map from variables to types
      * @param m coinduction hypothesis of module checking
      * @param i map from types to variables (generated by implicit resolution) of this type
      * @param shape expected shape of the type, normal form ? resolution ? transform cannot be called on shape
      * @return either term and types with resolved implicits or an error message
      */
    def transform(g: G, m: M, i: I, shape: Option[Term]): Either[String, (Term, Term)] =
        shape match
            case None => println(s"[DEBUG] $this")
            case Some(value) => println(s"[DEBUG] $this : $value")
        // typecheck and transform the shape
        if this == Typ && shape.contains(Typ) then Right(Typ, Typ)
        else shape.map(_.transform(g, m, i, Some(Typ)).map(_(0).eval)) match
            // add implicit in context // SHOULD t1 be checked to be nonempty ?
            case Some(Right(Imp(t1, t2))) =>
                t1.transform(g, m, i, Some(Typ)).flatMap((t1te, _) =>
                    (this >> 1).transform(g >> 1, m, i + (t1te -> Var(0, None)), Some(t2)).map((te, ty) => (Abs(te), Imp(t1, ty)))
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
                        (t.transform((g >> 1) + (0 -> t1), m >> 1, i >> 1, Some(t2))).map { (te, t3) =>
                            (Abs(te), Pro(t1, t3))
                        }
                    case _ => Left("type neccessary to type check abstraction")
                // typecheck t1 and use the result to typecheck t2
                case App(t1, t2) =>
                    t1.transform(g, m, i, None).flatMap {
                        case (abs, Pro(u1, u2)) =>
                            t2.transform(g, m, i, Some(u1)).flatMap { (t2te, t2ty) =>
                                App(abs, t2te).checkWithSearch((u2.replace(0, t2te >> 1) << 1), shape, i)
                            }
                        case (_, _) => Left(s"$t1 should have a product type")
                    }
                // parameter and result should be a type
                case Pro(t1, t2) =>
                    t1.transform(g, m, i, Some(Typ)).flatMap { (u1, _) =>
                        t2.transform((g >> 1) + (0 -> t1), m >> 1, i >> 1, Some(Typ)).flatMap { (u2, _) =>
                            Pro(u1, u2).checkWithSearch(Typ, shape, i)
                        }
                    }
                // search for implicit to match shape
                case Phi => shape match
                    case Some(value) => search(value, i, i).map(Right(_, value)).getOrElse(Left(s"no implicit found for $shape"))
                    case None => Left(s"no implicit found for $shape")
                // parameter and result should be a type
                case Imp(t1, t2) =>
                    t1.transform(g, m, i, Some(Typ)).flatMap { (u1, _) =>
                        t2.transform(g, m, i + (t1 -> Var(0, None)), Some(Typ)).flatMap { (u2, _) =>
                            Imp(u1, u2).checkWithSearch(Typ, shape, i)
                        }
                    }
                // check te against ty and then verify the shape
                case As(te, ty) => te.transform(g, m, i, Some(ty)).flatMap { (ue, uy) =>
                    ue.checkWithSearch(uy, shape, i)
                }

                //case mod: Module if m contains mod => Right(mod, m(mod))

                // TODO consider shape
                case Module(fields) => shape match
                    case None => checkModule(fields, Nil, Nil, g >> 1, m >> 1, i >> 1, Namer("imp$"))
                    case Some(Interface(types)) => checkModuleWithInterface(fields.zip(types), Nil, Nil, g >> 1, m >> 1, i >> 1, Namer("imp$"))
                    case Some(_) => Left("you're dumb")
                // TODO consider shape
                case Interface(fields) => checkInterface(fields, Nil, g >> 1, m >> 1, i >> 1).flatMap { (te, ty) =>
                    te.checkWithSearch(ty, shape, i)
                }

                case Get(t, field) => t.transform(g, m, i, None).flatMap { 
                    case (te, Interface(fields)) => 
                        fields.find(_.ident.exists(_ == field)) match
                            case Some(IntElem.Named(name, typ)) => Right(Get(te, field), typ)
                            case _ => Left("no '$field' field")
                    case _ => Left("no '$field' field")
                }

    def checkModuleWithInterface(fields: List[(ModElem, IntElem)], module: List[ModElem], interface: List[IntElem], g: G, m: M, i: I, namer: Namer): Either[String, (Module, Interface)] = fields match
        case Nil => Right(Module(module.reverse), Interface(interface.reverse))
        case (ModElem.Named(name, term), IntElem.Named(_name, typ)) :: next if name == _name => 
            val impContext =  i ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            term.tag(0, Module(module) >> 1).transform((g >> 1) + (0 -> Interface(interface)), m + (Module(module) -> Interface(interface)), impContext, Some(typ)).flatMap { (te, ty) =>
                checkModuleWithInterface(next, ModElem.Named(name, te) :: module, IntElem.Named(name, ty) :: interface, g, m, i, namer)
            }
        case (ModElem.Imp(typ, term), IntElem.Imp(name, typ2)) :: next if typ === typ2 =>
            val impContext =  i ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            typ.transform((g >> 1) + (0 -> Interface(interface)), m + (Module(module) -> Interface(interface)), impContext, Some(Typ)).flatMap { (u, _) => 
                term.tag(0, Module(module) >> 1).transform((g >> 1) + (0 -> Interface(interface)), m + (Module(module) -> Interface(interface)), impContext, Some(u)).flatMap { (te, ty) =>
                    checkModuleWithInterface(next, ModElem.Named(name, te) :: module, IntElem.Imp(name, ty) :: interface, g, m, i, namer)
                }
            }
    
    def checkModule(fields: List[ModElem], module: List[ModElem], interface: List[IntElem], g: G, m: M, i: I, namer: Namer): Either[String, (Module, Interface)] = fields match
        case Nil => Right(Module(module.reverse), Interface(interface.reverse))
        case ModElem.Named(name, term) :: next => 
            val impContext =  i ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            term.tag(0, Module(module) >> 1).transform((g >> 1) + (0 -> Interface(interface)), m + (Module(module) -> Interface(interface)), impContext, None).flatMap { (te, ty) =>
                checkModule(next, ModElem.Named(name, te) :: module, IntElem.Named(name, ty) :: interface, g, m, i, namer)
            }
        case ModElem.Imp(typ, term) :: next =>
            val impContext =  i ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            typ.transform((g >> 1) + (0 -> Interface(interface)), m + (Module(module) -> Interface(interface)), impContext, Some(Typ)).flatMap { (u, _) => 
                term.tag(0, Module(module) >> 1).transform((g >> 1) + (0 -> Interface(interface)), m + (Module(module) -> Interface(interface)), impContext, Some(u)).flatMap { (te, ty) =>
                    val name = namer.next()
                    checkModule(next, ModElem.Named(name, te) :: module, IntElem.Imp(name, ty) :: interface, g, m, i, namer)
                }
            }

    def checkInterface(fields: List[IntElem], checked: List[IntElem], g: G, m: M, i: I): Either[String, (Interface, Term.Typ.type)] = fields match
        case Nil => Right(Interface(checked.reverse), Typ)
        case IntElem.Named(name, typ) :: next =>
            val impContext =  i ++ checked.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            typ.transform((g >> 1) + (0 -> Interface(checked)), m, impContext, Some(Typ)).flatMap { (ty, _) => 
                checkInterface(next, IntElem.Named(name, ty) :: checked, g, m, i)
            }
        case IntElem.Imp(name, typ) :: next =>
            val impContext =  i ++ checked.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0, None), name)) }
            typ.transform((g >> 1) + (0 -> Interface(checked)), m, impContext, Some(Typ)).flatMap { (ty, _) => 
                checkInterface(next, IntElem.Imp(name, ty) :: checked, g, m, i)
            }

    def typeCheck = transform(Map.empty, Map.empty, Map.empty, None)
    
    def typeCheck(ty: Term) = transform(Map.empty, Map.empty, Map.empty, Some(ty))

}