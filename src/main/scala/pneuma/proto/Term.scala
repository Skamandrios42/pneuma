package pneuma.proto

import scala.annotation.targetName

enum Term {
    case Var(x: Int)
    case Abs(t: Term)
    case App(t1: Term, t2: Term)
    case Typ
    case Phi
    case Pro(t1: Term, t2: Term)
    case Imp(t1: Term, t2: Term)
    case Mod(fields: Map[String, Term], implicits: Map[Term, Term])
    case Interface(fields: Map[String, Term], implicits: Set[Term])
    case Get(t: Term, field: String)
    case Empty
    case As(te: Term, ty: Term)

    override def toString: String = this match
        case Term.Var(x) => s"$x"
        case Term.Abs(t) => s"(λ.$t)"
        case Term.App(t1, t2) => s"($t1 $t2)"
        case Term.Typ => "*"
        case Term.Phi => "φ"
        case Term.Pro(t1, t2) => s"(π$t1.$t2)"
        case Term.Imp(t1, t2) => s"(φ$t1.$t2)"
        case Term.Mod(fields, implicits) =>
            val fieldsStr = fields map { case (k, v) => s"$k = $v"} mkString ", "
            val implicitsStr = implicits map { t => s"φ = $t" } mkString ", "
            s"{ $fieldsStr, $implicitsStr }"
        case Term.Interface(fields, implicits) =>
            val fieldsStr = fields map { case (k, v) => s"$k : $v"} mkString ", "
            val implicitsStr = implicits map { t => s"φ : $t" } mkString ", "
            s"{ $fieldsStr, $implicitsStr }"
        case Term.Get(t, field) => s"($t.$field)"
        case Term.Empty => " ? "
        case Term.As(te, ty) => s"($te : $ty)"


    extension [K, V](self: Map[K, V]) {
        def mapV[W](f: V => W): Map[K, W] = self.map { case (key, value) => (key, f(value)) }
    }
    extension [A](self: Map[A, A]) {
        def mapB[B](f: A => B): Map[B, B] = self.map { case (key, value) => (f(key), f(value)) }
    }

    def shift(amount: Int, cutoff: Int): Term = this match
        case Var(x) => if x >= cutoff then Var(x + amount) else Var(x)
        case Abs(t) => Abs(t.shift(amount, cutoff + 1))
        case App(t1, t2) => App(t1.shift(amount, cutoff), t2.shift(amount, cutoff))
        case Typ => Typ
        case Phi => Phi
        case Pro(t1, t2) => Pro(t1, t2.shift(amount, cutoff + 1))
        case Imp(t1, t2) => Imp(t1, t2.shift(amount, cutoff + 1))
        case Mod(fields, implicits) => Mod(fields.mapV(_.shift(amount, cutoff + 1)), implicits.mapB(_.shift(amount, cutoff + 1)))
        case Interface(fields, implicits) => Interface(fields.mapV(_.shift(amount, cutoff + 1)), implicits.map(_.shift(amount, cutoff + 1)))
        case Get(t, field) => Get(t.shift(amount, cutoff), field)
        case Empty => Empty
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
        case Mod(fields, implicits) => Mod(fields.mapV(_.replace(y + 1, t >> 1)), implicits.mapB(_.replace(y + 1, t >> 1)))
        case Interface(fields, implicits) => Interface(fields.mapV(_.replace(y + 1, t >> 1)), implicits.map(_.replace(y + 1, t >> 1)))
        case Get(t1, field) => Get(t1.replace(y, t), field)
        case Empty => Empty
        case Term.As(te, ty) => Term.As(te.replace(y, t), ty.replace(y, t))


    type G = Map[Int, Term]
    type M = Map[Term, Term]
    type P = Set[Term]

    extension (self: G) {
        @targetName("shiftG")
        def >>(amount: Int): G = self.map { case (key, value) => (key + amount, value >> amount) }
    }

    extension (self: M) {
        @targetName("shiftM")
        def >>(amount: Int): M = self.map { case (key, value) => (key >> amount, value >> amount) }
    }

    extension (self: P) {
        @targetName("shiftP")
        def >>(amount: Int): P = self.map { _ >> amount }
    }

    case class Typing(te: Term, ty: Term)

    def eval: Term = this match
        case Term.App(t1, t2) => (t1.eval, t2.eval) match
            case (Term.Abs(body), arg) => (body.replace(0, arg >> 1) << 1).eval
            case (abs, arg) => Term.App(abs, arg)
        case Term.Get(t, field) => t.eval match
            case mod @ Mod(fields, implicits) if fields.contains(field) => (fields(field).replace(0, mod >> 1) << 1).eval
            case mod => Term.Get(mod, field)
        case Term.As(te, ty) => te
        case _ => this

    def ===(that: Term): Boolean = this == that
    def <==(that: Term): Boolean =
        println("METHOD STUB called: [default = true]")
        true

    def check(shape: Term) = if this <== shape then Right(this) else Left(s"$this does not match $shape")

    def query(typ: Term, p: P, all: P): Boolean = p.headOption match
        case Some(imp) if imp === typ => true
        case Some(Imp(t1, t2)) if query(t1, all, all) => true
        case Some(_) => query(typ, p.tail, all)
        case None => false

    def typeCheck(expected: Term): Either[String, Term] = typeCheck(Map.empty, Map.empty, Set.empty, expected)

    def typeCheck(g: G, m: M, p: P, shape: Term): Either[String, Term] = shape match

        // add implicit in context // SHOULD t1 be checked to be nonempty ?
        case Imp(t1, t2) => this.typeCheck(g, m, p + t1, t2).map { te => Imp(t1, te) }

        // otherwise match on this and apply typechecking rules
        case _           => this match

            // checks if Typ matches shape
            case Typ    => Typ check shape

            // get type from context
            case Var(x) => g get x match
                // checks if variable matches shape
                case Some(value) => value check shape // TRIGGER search if value expects implicit but shouldn't do so
                // report undefined variable type
                case None => Left("undefined variable")

            // get product type from shape
            case Abs(t) => shape.eval match // WILL SHAPE BE NORMAL-FORM? I think not e. g. because of t2  (... evaluate with implicits inserted !!)
                case Pro(t1, t2) => // SHOULD t1 be checked to be nonempty
                    for t3 <- t.typeCheck((g >> 1) + (0 -> t1), m >> 1, p >> 1, t2)
                    yield Pro(t1, t3)
                case _ => Left("type neccessary to type check abstraction")

            // typecheck t1 and proceed from there-on
            case App(t1, t2) => // TRIGGER implicit search ?? -- maybe it shouldn't because Empty shape should prevent implicit type here
                t1.typeCheck(g, m, p, Empty).flatMap {
                    case Pro(u1, u2) =>
                        t2.typeCheck(g, m, p, u1).flatMap { _ =>
                            (u2.replace(0, t2 >> 1) << 1) check shape // search triggered implicitly by check?
                        }
                    case _ => Left(s"$t1 should have a product type")
                }

            // parameter and result should be a type
            case Pro(t1, t2) =>
                t1.typeCheck(g, m, p, Typ).flatMap { _ =>
                    t2.typeCheck((g >> 1) + (0 -> t1), m >> 1, p >> 1, Typ).flatMap { _ =>
                        Typ check shape
                    }
                }

            // search for implicit to match shape
            case Phi => if query(shape, p, p) then Right(shape) else Left(s"no implicit found for $shape")

            // parameter and result should be a type
            case Imp(t1, t2) =>
                t1.typeCheck(g, m, p, Typ).flatMap { _ =>
                    t2.typeCheck(g, m, p + t1, Typ).flatMap { _ =>
                        Typ check shape
                    }
                }

            // check te against ty and then verify the shape
            case Term.As(te, ty) => te.typeCheck(g, m, p, ty).flatMap { _ check shape }

            // no valid term -- only for type shapes
            case Empty => Left(s"empty shape has no type") // RETURN SHAPE ?

            // module rules
            case mod: Mod if m contains mod => Right(m(mod)) // take the module type from the module-context

            case mod @ Mod(fields, implicits) => shape.eval match
                // required interface type
                case int @ Interface(fieldsT, implicitsT) =>
                    // build the type of the fields
                    val fieldsTy = fields.keys.foldLeft[Either[String, Map[String, Term]]](Right(Map.empty)) {
                        case (Right(coll), key) =>
                            val fieldTe = fields(key)
                            val fieldTy = fieldsT(key)
                            val newImps = (p ++ implicits.keySet)
                            fieldTe.replace(0, this >> 1).typeCheck(g, m + (mod -> int), newImps, fieldTy) map { ty =>
                                coll + (key -> ty)
                            }
                        case (Left(error), _) => Left(error)
                    }
                    // build the types of the implicits
                    val implicitsTy = implicits.foldLeft[Either[String, Set[Term]]](Right(Set.empty)) {
                        case (Right(coll), ty -> te) =>
                            val newImps = (p ++ implicits.keySet)
                            te.replace(0, this >> 1).typeCheck(g, m + (mod -> int), newImps, ty) map { coll + _ }
                        case (Left(error), _) => Left(error)
                    }
                    // combine the implicit types and the fields types into the interface
                    fieldsTy.flatMap { fty =>
                        implicitsTy.flatMap { ity =>
                            if ity == implicitsT // different equality SHOULD type contain information about implicits?
                            then Right(Interface(fty, ity))
                            else Left(s"modules implicits do not match interface: $ity, $implicitsT")
                        }
                    }
                case _ => Left("interface type needed to check module")

            case int @ Interface(fields, implicits) =>
                if  fields.forall { field => field(1).typeCheck((g >> 1) + (0 -> int), m >> 1, p >> 1, Typ).isRight } &&
                    implicits.forall { _.typeCheck((g >> 1) + (0 -> int), m >> 1, p >> 1, Typ).isRight }
                then Right(Typ) else Left("malformed interface")

            case Get(t, field) =>
                t.typeCheck(g, m, p, Empty).flatMap {
                    case Interface(fields, implicits) => fields get field match
                        case Some(value) if value <== shape => Right(value)
                        case Some(value) => Left(s"$value does not fulfill $shape")
                        case None => Left("undefined field")
                    case _ => Left(s"expected interface")
                }

}
