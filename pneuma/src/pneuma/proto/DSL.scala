package pneuma.proto

import scala.language.implicitConversions

/** a small _DSL_ for [[Term]] */
object DSL {

    val \ = Term.Abs
    given Conversion[Int, Term.Var] = Term.Var(_, None)
    val ? = Term.Phi
    val * = Term.Typ
    inline def nat(inline value: Int) = 
        inline if value >= 0 then Term.Nat(value) else compiletime.error("natural numbers should be non-negative")
    def Nat = Term.NatType
    
    extension (self: Term) {
        def apply(term: Term) = Term.App(self, term)
        def at(term: Term) = Term.App(self, term)
        def as(term: Term) = Term.As(self, term)
        def -->(term: Term) = Term.Pro(self, term)
        def -?>(term: Term) = Term.Imp(self, term)
        def !(field: String) = Term.Get(self, field)
        def succ = Term.Succ(self)
        def pattern(onZero: Term, onSucc: Term) = Term.Match(self, onZero, onSucc)
    }

    extension (self: String) {
        def :=(term: Term) = Term.ModElem(self, term, Term.Mode.Exp)
        def ::=(term: Term) = Term.IntElem(self, term, Term.Mode.Exp)
        def :?=(term: Term) = Term.ModElem(self, term, Term.Mode.Imp)
        def ::?=(term: Term) = Term.IntElem(self, term, Term.Mode.Imp)
    }

    def mod(defs: Term.ModElem*) = Term.Module(defs.toList)
    def int(defs: Term.IntElem*) = Term.Interface(defs.toList)

}
