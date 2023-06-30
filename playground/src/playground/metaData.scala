package playground

import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*

case class Metadata(file: String, pos: Int)

trait HasMeta[T] {
    def metaImpl(t: T): Metadata
    def attachImpl(into: T, meta: Metadata): T
    extension (self: T) {
        def meta = metaImpl(self)
        def metaOf(that: T) = attachImpl(self, that.meta)
        def attach(meta: Metadata) = attachImpl(self, meta)
    }
}

object HasMeta {

    inline def derived[A] = ${ generateImpl[A] }

    def generateImpl[T](using qu: Quotes, ty: Type[T]) = 
        import quotes.reflect.*

        def recMeta(x: Expr[T], subs: List[Symbol]): Expr[Metadata] = subs match
            case head :: Nil =>
                Select.unique(TypeApply(Select.unique(x.asTerm, "asInstanceOf"), List(TypeTree.ref(head))), "meta").asExprOf[Metadata]
            case head :: next => 
                If(
                    TypeApply(Select.unique(x.asTerm, "isInstanceOf"), List(TypeTree.ref(head))), 
                    Select.unique(TypeApply(Select.unique(x.asTerm, "asInstanceOf"), List(TypeTree.ref(head))), "meta"), 
                    recMeta(x, next).asTerm
                ).asExprOf[Metadata]
            case Nil => throw Exception("no sub cases")

        def recAttach(self: Expr[HasMeta[T]], into: Expr[T], meta: Expr[Metadata], subs: List[Symbol], parent: Symbol): Expr[T] = subs match
            case head :: Nil => 
                val caseSymbol = Symbol.newVal(parent, "case$", TypeTree.ref(head).tpe, Flags.EmptyFlags, Symbol.noSymbol)
                Block(List(
                    ValDef(caseSymbol, Some(TypeApply(Select.unique(into.asTerm, "asInstanceOf"), List(TypeTree.ref(head)))))
                ), Apply(
                    Select.unique(Ident(head.companionModule.termRef), "apply"), 
                    NamedArg("meta", meta.asTerm //Apply(Select.unique(self.asTerm, "metaImpl"), List(from.asTerm))
                    ) :: 
                    head.caseFields.filter(_.name != "meta").map(sym => NamedArg(sym.name, Select(Ident(caseSymbol.termRef), sym)))
                )).asExprOf[T]
            case head :: next => 
                val caseSymbol = Symbol.newVal(parent, "case$", TypeTree.ref(head).tpe, Flags.EmptyFlags, Symbol.noSymbol)
                If(
                    TypeApply(Select.unique(into.asTerm, "isInstanceOf"), List(TypeTree.ref(head))), 
                    Block(List(
                        ValDef(caseSymbol, Some(TypeApply(Select.unique(into.asTerm, "asInstanceOf"), List(TypeTree.ref(head)))))
                    ), Apply(
                        Select.unique(Ident(head.companionModule.termRef), "apply"), 
                        NamedArg("meta", meta.asTerm //Apply(Select.unique(self.asTerm, "metaImpl"), List(from.asTerm))
                        ) :: 
                        head.caseFields.filter(_.name != "meta").map(sym => NamedArg(sym.name, Select(Ident(caseSymbol.termRef), sym)))
                    )),
                    (recAttach(self, into, meta, next, parent)).asTerm
                ).asExprOf[T]
            case Nil => throw Exception("no sub cases")

        '{
            lazy val impl: HasMeta[T] = new HasMeta[T] {
                def metaImpl(t: T): Metadata = ${ recMeta('t, TypeRepr.of[T].typeSymbol.children) }
                def attachImpl(into: T, meta: Metadata): T = ${ recAttach('impl, 'into, 'meta, TypeRepr.of[T].typeSymbol.children, '{impl}.asTerm.symbol.methodMember("attachImpl").head) }
            }

            impl
        }
}
