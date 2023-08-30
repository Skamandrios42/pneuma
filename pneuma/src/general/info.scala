package general

import java.nio.file.Path
import java.io.File
import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*

object Metadata {
    def none = Metadata(None, 0, 0)
}
case class Metadata(file: Option[String], start: Int, end: Int) {

    def extract(content: String) =
        content.drop(start).dropRight(content.length() - end)
    def extract(content: Seq[String]) = 
        val startPos = Pos.from(start, content)
        val endPos = Pos.from(end, content)
        val lines = content.slice(startPos.line, endPos.line)
        lines.updated(0, lines.head.drop(startPos.char)).updated(lines.length - 1, lines.last.take(endPos.char))

    def format(content: Seq[String]) = 
        val pre = file.map(_ ++ ", ").getOrElse("")
        if start == end
        then s"$pre${Pos.from(start, content)}"
        else s"$pre${Pos.from(start, content)} to $pre${Pos.from(end, content)}"

    def join(other: Metadata) = Metadata(file, this.start, other.end)

    def mark(content: Seq[String], indent: Int) = {
        val startPos = Pos.from(start, content)
        val endPos = Pos.from(end, content)
        if startPos == endPos then
            val line = (" " * indent) ++ content(startPos.line)
            val marker = (" " * (startPos.char + indent)) ++ "^"
            line ++ "\n" ++ marker
        else if startPos.line == endPos.line then 
            val len = (endPos.char - startPos.char)
            val line = content(startPos.line)
            val marker = (" " * (startPos.char + indent)) ++ ("^" * len)
            val colored = (" " * indent) ++ line.take(startPos.char) ++ Console.RED ++ line.drop(startPos.char).take(len) ++ Console.RESET ++ line.drop(startPos.char + len)
            colored ++ "\n" ++ marker
        else
            val len = endPos.char + 4
            val line = "... " ++ content(endPos.line)
            val marker = (" " * indent) ++ ("^" * len)
            val colored = (" " * indent) ++ Console.RED ++ line.take(len) ++ Console.RESET ++ line.drop(len)
            colored ++ "\n" ++ marker
    }

    override def toString: String = file match
        case None => s"$start to $end"
        case Some(value) => s"$value, $start to $end"
}

trait HasMeta[T] {
    def metaImpl(t: T): Metadata
    def attachImpl(into: T, meta: Metadata): T
    extension (self: T) {
        inline def meta = metaImpl(self)
        inline def metaOf(inline that: T) = attachImpl(self, that.meta)
        inline def attach(inline meta: Metadata) = attachImpl(self, meta)
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

object Pos {
    def from(index: Int, content: Seq[String]) =
        var i = index
        var line = 0
        var char = 0
        var done = false
        for s <- content if !done do
            if s.length < i 
            then
                i -= s.length + 1
                line += 1
            else
                char = i
                done = true
        Pos(line, char)
}

case class Pos(line: Int, char: Int) {
    override def toString: String = s"$line:$char"
}
