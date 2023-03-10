package pneuma.proto

import scala.language.implicitConversions

object DSL {

    val \ = Term.Abs
    given Conversion[Int, Term.Var] = Term.Var(_)
    val ? = Term.Phi
    val * = Term.Typ

    extension (self: Term) {
        def apply(term: Term) = Term.App(self, term)
        def at(term: Term) = Term.App(self, term)
        def as(term: Term) = Term.As(self, term)
        def -->(term: Term) = Term.Pro(self, term)
        def -?>(term: Term) = Term.Imp(self, term)
        def !(field: String) = Term.Get(self, field)
    }

    //case class Def(field: String, term: Term)

    extension (self: String) {
        def :=(term: Term) = ModElem.Named(self, term)
        def ::=(term: Term) = IntElem.Named(self, term)
    }
    extension (self: Term) {
        def ?=(term: Term) = ModElem.Imp(self, term)
    }

    given Conversion[Term, IntElem] = IntElem.Imp(_)

    def mod(defs: ModElem*) = Term.Module(defs.toList)
    def int(defs: IntElem*) = Term.Interface(defs.toList)

}

// def test_1(): Unit =
//     import DSL.{*, given}
//     val te = \(0 at ?)
//     val ty = (* --> *) --> (* -?> *)
//     te.typeCheck(expected = ty) match
//         case Left(value) => println(value)
//         case Right(value) => println(value)

// def test_2(): Unit =
//     import DSL.{*, given}
//     val te: Term = mod (
//         "x" := *,
//         "y" := (* --> *),
//     )()
//     val ty: Term = int (
//         "x" := *,
//         "y" := (0 ! "x")
//     )()
//     te.typeCheck(expected = ty) match
//         case Left(value) => println(value)
//         case Right(value) => println(value)

// @main def tests(): Unit =
//     test_1()
//     test_2()
