package general

import java.nio.file.Path
import java.io.File

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

    def join(other: Region) = Region(file, start, other.end)

    override def toString: String = file match
        case None => s"$start to $end"
        case Some(value) => s"$value, $start to $end"
}

object Pos {
    def from(index: Int, content: Seq[String]) =
        var i = index
        var line = 0
        var char = 0
        for s <- content do
            if s.length < i then
                i -= s.length
                line += 1
            else char = i
        Pos(line, char)
}

case class Pos(line: Int, char: Int) {
    override def toString: String = s"$line:$char"
}
