package pneuma.proto

import scala.language.implicitConversions

object DSL {

    val \ = Term.Abs
    given Conversion[Int, Term.Var] = Term.Var(_, None)
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

    extension (self: String) {
        def :=(term: Term) = ModElem.Named(self, term)
        def ::=(term: Term) = IntElem.Named(self, term)
        def ?=(term: Term) = IntElem.Imp(self, term)
    }
    extension (self: Term) {
        def ?=(term: Term) = ModElem.Imp(self, term)
    }

    def mod(defs: ModElem*) = Term.Module(defs.toList)
    def int(defs: IntElem*) = Term.Interface(defs.toList)

}
