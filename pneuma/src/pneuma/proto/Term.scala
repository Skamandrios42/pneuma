package pneuma.proto

import scala.annotation.targetName
import lambdacalculus.Generator

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
    case Var(x: Int)
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
        case Term.Var(x) => s"$x"
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
        case Var(x) => if x >= cutoff then Var(x + amount) else Var(x)
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
        case Var(x) => if x == y then t else Var(x)
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
        println(s"debug: [$this] [$shape]")
        // typecheck and transform the shape
        if this == Typ && shape.contains(Typ) then Right(Typ, Typ)
        else shape.map(_.transform(g, m, i, Some(Typ)).map(_(0).eval)) match
            // add implicit in context // SHOULD t1 be checked to be nonempty ?
            case Some(Right(Imp(t1, t2))) =>
                t1.transform(g, m, i, Some(Typ)).flatMap((t1te, _) =>
                    (this >> 1).transform(g >> 1, m, i + (t1te -> Var(0)), Some(t2)).map((te, ty) => (Abs(te), Imp(t1, ty)))
                )
            // otherwise match on this and apply typechecking rules
            case _           => this match
                // checks if Typ matches shape
                case Typ => Typ.checkWithSearch(Typ, shape, i)
                // get type from context
                case Var(x) => g get x match
                    // checks if variable matches shape
                    case Some(value) => Var(x).checkWithSearch(value, shape, i)
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
                        t2.transform(g, m, i + (t1 -> Var(0)), Some(Typ)).flatMap { (u2, _) =>
                            Imp(u1, u2).checkWithSearch(Typ, shape, i)
                        }
                    }
                // check te against ty and then verify the shape
                case As(te, ty) => te.transform(g, m, i, Some(ty)).flatMap { (ue, uy) =>
                    ue.checkWithSearch(uy, shape, i)
                }

                case mod: Module if m contains mod => Right(mod, m(mod))

                // TODO consider shape
                case Module(fields) => checkModuleTailRecursive(fields, Nil, Nil, g, m, i, Generator.Namer("imp$"))
                // TODO consider shape
                case Interface(fields) => checkInterfaceTailRecursive(fields, Nil, g, m, i)

                case Get(t, field) => t.transform(g, m, i, None).flatMap { 
                    case (te, Interface(fields)) => 
                        fields.find(_.ident.exists(_ == field)) match
                            case Some(IntElem.Named(name, typ)) => Right(Get(te, field), typ)
                            case _ => Left("no '$field' field")
                    case _ => Left("no '$field' field")
                }

    def checkModuleTailRecursive(fields: List[ModElem], module: List[ModElem], interface: List[IntElem], g: G, m: M, i: I, namer: Generator.Namer): Either[String, (Module, Interface)] = fields match
        case Nil => Right(Module(module.reverse), Interface(interface.reverse))
        case ModElem.Named(name, term) :: next => 
            val impContext =  i ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0), name)) }
            term.replace(0, Module(module) >> 1).transform(g, m + (Module(module) -> Interface(interface)), impContext, None).flatMap { (te, ty) =>
                checkModuleTailRecursive(next, ModElem.Named(name, te) :: module, IntElem.Named(name, ty) :: interface, g, m, i, namer)
            }
        case ModElem.Imp(typ, term) :: next =>
            val impContext =  i ++ interface.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0), name)) }
            typ.replace(0, Module(module) >> 1).transform(g, m + (Module(module) -> Interface(interface)), impContext, Some(Typ)).flatMap { (u, _) => 
                term.replace(0, Module(module) >> 1).transform(g, m + (Module(module) -> Interface(interface)), impContext, Some(u)).flatMap { (te, ty) =>
                    val name = namer.next()
                    checkModuleTailRecursive(next, ModElem.Named(name, te) :: module, IntElem.Imp(name, ty) :: interface, g, m, i, namer)
                }
            }

    def checkInterfaceTailRecursive(fields: List[IntElem], checked: List[IntElem], g: G, m: M, i: I): Either[String, (Interface, Term.Typ.type)] = fields match
        case Nil => Right(Interface(checked.reverse), Typ)
        case IntElem.Named(name, typ) :: next =>
            val impContext =  i ++ checked.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0), name)) }
            typ.transform((g >> 1) + (0 -> Interface(checked)), m, impContext, Some(Typ)).flatMap { (ty, _) => 
                checkInterfaceTailRecursive(next, IntElem.Named(name, ty) :: checked, g, m, i)
            }
        case IntElem.Imp(name, typ) :: next =>
            val impContext =  i ++ checked.collect { case IntElem.Imp(name, typ) => (typ, Get(Var(0), name)) }
            typ.transform((g >> 1) + (0 -> Interface(checked)), m, impContext, Some(Typ)).flatMap { (ty, _) => 
                checkInterfaceTailRecursive(next, IntElem.Imp(name, ty) :: checked, g, m, i)
            }

    def typeCheck = transform(Map.empty, Map.empty, Map.empty, None)
    def typeCheck(ty: Term) = transform(Map.empty, Map.empty, Map.empty, Some(ty))

    
    // def checkModuleAsRecord(fields: List[ModElem], g: G, m: M, i: I, namer: Generator.Namer): Either[String, (Module, Interface)] = fields match
    //     case Nil => Right(Module(Nil), Interface(Nil))
    //     case ModElem.Named(name, term) :: next => 
    //         term.transform(g, m, i, None).flatMap { (te, ty) =>
    //             checkModuleAsRecord(next, g, m, i, namer).map { (mod, int) =>
    //                 (Module(ModElem.Named(name, te) :: mod.fields), Interface(IntElem.Named(name, ty) :: int.fields))
    //             }
    //         }
    //     case ModElem.Imp(typ, term) :: next =>
    //         typ.transform(g, m, i, Some(Typ)).flatMap { (u, _) => 
    //             term.transform(g, m, i, Some(u)).flatMap { (te, ty) =>
    //                 checkModuleAsRecord(next, g, m, i, namer).map { (mod, int) =>
    //                     val name = namer.next()
    //                     (Module(ModElem.Named(name, te) :: mod.fields), Interface(IntElem.Named(name, ty) :: int.fields))
    //                 }
    //             }
    //         }
    
    // def checkModule(fields: List[ModElem], g: G, m: M, i: I, shape: List[IntElem]): Either[String, (Term, Term)] = ???



/*


name = term


*/









                // TODO how will implicits of mod be resolved ?
                // case mod: Mod if m contains mod => Right(mod, m(mod)) // take the module type from the module-context
                // case mod @ Mod(fields, implicits) => shape match
                //     case Some(int @ Interface(types, imps)) =>
                //         // build the type of the fields
                //         val fieldTypes = fields.keys.foldLeft[Either[String, Map[String, Term]]](Right(Map.empty)) {
                //             case (Right(coll), key) =>
                //                 val fieldTe = fields(key)
                //                 val fieldTy = types(key)
                //                 val newImps = (i ++ implicits)
                //                 fieldTe.replace(0, this >> 1).transform(g, m + (mod -> int), i, Some(fieldTy)) map { ty =>
                //                     coll + (key -> ty)
                //                 }
                //             case (Left(error), _) => Left(error)
                //         }
                //         // build the types of the implicits
                //         val implicitsTy = implicits.foldLeft[Either[String, Set[Term]]](Right(Set.empty)) {
                //             case (Right(coll), ty -> te) =>
                //                 val newImps = (p ++ implicits.keySet)
                //                 te.replace(0, this >> 1).typeCheck(g, m + (mod -> int), newImps, ty) map { coll + _ }
                //             case (Left(error), _) => Left(error)
                //         }
                //         // combine the implicit types and the fields types into the interface
                //         fieldTypes.flatMap { fty =>
                //             implicitsTy.flatMap { ity =>
                //                 if ity == implicitsT // different equality SHOULD type contain information about implicits?
                //                 then Right(Interface(fty, ity))
                //                 else Left(s"modules implicits do not match interface: $ity, $implicitsT")
                //             }
                //         }


                //     case None => Left("interface type needed to check module")

                // case Interface(fields, implicits) => ???
                // case Get(t, field) => ???







    // def typeCheck(expected: Term): Either[String, Term] = typeCheck(Map.empty, Map.empty, Set.empty, expected)

//     def typeCheck(g: G, m: M, p: P, shape: Term): Either[String, Term] = shape match

//         // add implicit in context // SHOULD t1 be checked to be nonempty ?
//         case Imp(t1, t2) => this.typeCheck(g, m, p + t1, t2).map { res => Imp(t1, res) }

//         // otherwise match on this and apply typechecking rules
//         case _           => this match

//             // checks if Typ matches shape
//             case Typ    => Typ check shape

//             // get type from context
//             case Var(x) => g get x match
//                 // checks if variable matches shape
//                 case Some(value) => value check shape // TRIGGER search if value expects implicit but shouldn't do so
//                 // report undefined variable type
//                 case None => Left("undefined variable")

//             // get product type from shape
//             case Abs(t) => shape.eval match // WILL SHAPE BE NORMAL-FORM? I think not e. g. because of t2  (... evaluate with implicits inserted !!)
//                 case Pro(t1, t2) => // SHOULD t1 be checked to be nonempty
//                     for t3 <- t.typeCheck((g >> 1) + (0 -> t1), m >> 1, p >> 1, t2)
//                     yield Pro(t1, t3)
//                 case _ => Left("type neccessary to type check abstraction")

//             // typecheck t1 and proceed from there-on
//             case App(t1, t2) => // TRIGGER implicit search ?? -- maybe it shouldn't because Empty shape should prevent implicit type here
//                 t1.typeCheck(g, m, p, ???).flatMap {
//                     case Pro(u1, u2) =>
//                         t2.typeCheck(g, m, p, u1).flatMap { _ =>
//                             (u2.replace(0, t2 >> 1) << 1) check shape // search triggered implicitly by check?
//                         }
//                     case _ => Left(s"$t1 should have a product type")
//                 }

//             // parameter and result should be a type
//             case Pro(t1, t2) =>
//                 t1.typeCheck(g, m, p, Typ).flatMap { _ =>
//                     t2.typeCheck((g >> 1) + (0 -> t1), m >> 1, p >> 1, Typ).flatMap { _ =>
//                         Typ check shape
//                     }
//                 }

//             // search for implicit to match shape
//             case Phi => if query(shape, p, p) then Right(shape) else Left(s"no implicit found for $shape")

//             // parameter and result should be a type
//             case Imp(t1, t2) =>
//                 t1.typeCheck(g, m, p, Typ).flatMap { _ =>
//                     t2.typeCheck(g, m, p + t1, Typ).flatMap { _ =>
//                         Typ check shape
//                     }
//                 }

//             // check te against ty and then verify the shape
//             case Term.As(te, ty) => te.typeCheck(g, m, p, ty).flatMap { _ check shape }

//             // no valid term -- only for type shapes
//             //case Empty => Left(s"empty shape has no type") // RETURN SHAPE ?

//             // module rules
//             case mod: Module if m contains mod => Right(m(mod)) // take the module type from the module-context

//             case mod @ Module(fields, implicits) => shape.eval match
//                 // required interface type
//                 case int @ Interface(fieldsT, implicitsT) =>
//                     // build the type of the fields
//                     val fieldsTy = fields.keys.foldLeft[Either[String, Map[String, Term]]](Right(Map.empty)) {
//                         case (Right(coll), key) =>
//                             val fieldTe = fields(key)
//                             val fieldTy = fieldsT(key)
//                             val newImps = (p ++ implicits.keySet)
//                             fieldTe.replace(0, this >> 1).typeCheck(g, m + (mod -> int), newImps, fieldTy) map { ty =>
//                                 coll + (key -> ty)
//                             }
//                         case (Left(error), _) => Left(error)
//                     }
//                     // build the types of the implicits
//                     val implicitsTy = implicits.foldLeft[Either[String, Set[Term]]](Right(Set.empty)) {
//                         case (Right(coll), ty -> te) =>
//                             val newImps = (p ++ implicits.keySet)
//                             te.replace(0, this >> 1).typeCheck(g, m + (mod -> int), newImps, ty) map { coll + _ }
//                         case (Left(error), _) => Left(error)
//                     }
//                     // combine the implicit types and the fields types into the interface
//                     fieldsTy.flatMap { fty =>
//                         implicitsTy.flatMap { ity =>
//                             if ity == implicitsT // different equality SHOULD type contain information about implicits?
//                             then Right(Interface(fty, ity))
//                             else Left(s"modules implicits do not match interface: $ity, $implicitsT")
//                         }
//                     }
//                 case _ => Left("interface type needed to check module")

//             case int @ Interface(fields, implicits) =>
//                 if  fields.forall { field => field(1).typeCheck((g >> 1) + (0 -> int), m >> 1, p >> 1, Typ).isRight } &&
//                     implicits.forall { _.typeCheck((g >> 1) + (0 -> int), m >> 1, p >> 1, Typ).isRight }
//                 then Right(Typ) else Left("malformed interface")

//             case Get(t, field) =>
//                 t.typeCheck(g, m, p, ???).flatMap {
//                     case Interface(fields, implicits) => fields get field match
//                         case Some(value) if value <== shape => Right(value)
//                         case Some(value) => Left(s"$value does not fulfill $shape")
//                         case None => Left("undefined field")
//                     case _ => Left(s"expected interface")
//                 }

}