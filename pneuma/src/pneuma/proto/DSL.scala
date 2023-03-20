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
        def :=(term: Term) = ModElem.Named(self, term)
        def ::=(term: Term) = IntElem.Named(self, term)
        def ?=(term: Term) = IntElem.Imp(self, term)
        def ?:(term: Term) = (self, term)
    }
    extension (self: (String, Term)) {
        def ?=(term: Term) = ModElem.Imp(self(0), self(1), term)
    }

    def mod(defs: ModElem*) = Term.Module(defs.toList)
    def int(defs: IntElem*) = Term.Interface(defs.toList)

}
