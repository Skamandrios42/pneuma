package pneuma.proto

import scala.language.implicitConversions
import general.Metadata

/** a small _DSL_ for [[Term]] */
object DSL {

    def \(t: Term) = Term.Abs(t, Metadata(None, 0, 0), "")
    given Conversion[Int, Term.Var] = Term.Var(_, None, Metadata(None, 0, 0))
    val ? = Term.Phi(Metadata(None, 0, 0))
    val * = Term.Typ(Metadata(None, 0, 0))
    inline def nat(inline value: Int) = 
        inline if value >= 0 then Term.Nat(value, Metadata(None, 0, 0)) else compiletime.error("natural numbers should be non-negative")
    def Nat = Term.NatType(Metadata(None, 0, 0))

    extension (self: Term) {
        def apply(term: Term) = Term.App(self, term, Metadata(None, 0, 0))
        def at(term: Term) = Term.App(self, term, Metadata(None, 0, 0))
        def as(term: Term) = Term.As(self, term, Metadata(None, 0, 0))
        def -->(term: Term) = Term.Pro(self, term, Metadata(None, 0, 0), "")
        def -?>(term: Term) = Term.Imp(self, term, Metadata(None, 0, 0))
        def !(field: String) = Term.Get(self, field, Metadata(None, 0, 0))
        def succ = Term.Succ(self, Metadata(None, 0, 0))
        def debug = Term.Debug(self, Metadata(None, 0, 0))
        def pattern(onZero: Term, onSucc: Term) = Term.Match(self, onZero, onSucc, Metadata(None, 0, 0), "")
    }

    extension (self: String) {
        def :=(term: Term) = Term.ModElem(self, term, Term.Mode.Exp)
        def ::=(term: Term) = Term.IntElem(self, term, Term.Mode.Exp)
        def :?=(term: Term) = Term.ModElem(self, term, Term.Mode.Imp)
        def ::?=(term: Term) = Term.IntElem(self, term, Term.Mode.Imp)
    }

    def mod(defs: Term.ModElem*) = Term.Module(defs.toList, Metadata(None, 0, 0))
    def int(defs: Term.IntElem*) = Term.Interface(defs.toList, Metadata(None, 0, 0))

}
