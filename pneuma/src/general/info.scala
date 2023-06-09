package general

import java.nio.file.Path
import java.io.File

object Region {
    def none = Region(None, 0, 0)
}
case class Region(file: Option[String], start: Int, end: Int) {

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

    def join(other: Region) = Region(file, this.start, other.end)

    def mark(content: Seq[String], indent: Int) = {
        val startPos = Pos.from(start, content)
        val endPos = Pos.from(end, content)
        val len = (endPos.char - startPos.char)
        if startPos == endPos then
            val line = (" " * indent) ++ content(startPos.line)
            val marker = (" " * (startPos.char + indent)) ++ "^"
            line ++ "\n" ++ marker
        else if startPos.line == endPos.line then 
            val line = content(startPos.line)
            val marker = (" " * (startPos.char + indent)) ++ ("^" * len)
            val colored = (" " * indent) ++ line.take(startPos.char) ++ Console.RED ++ line.drop(startPos.char).take(len) ++ Console.RESET ++ line.drop(startPos.char + len)
            colored ++ "\n" ++ marker
        else " ... not implemented ..."
    }

    override def toString: String = file match
        case None => s"$start to $end"
        case Some(value) => s"$value, $start to $end"
}

trait HasRegion {
    def r: Region
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
