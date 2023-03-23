package general

import java.nio.file.Path
import java.io.File

case class Region(file: Option[File], start: Pos, end: Pos) {
    override def toString: String = file match
        case None => s"$start to $end"
        case Some(value) => s"${value.getName()}, $start to $end"
    def extract(content: Seq[String]) = 
        val lines = content.slice(start.line, end.line)
        lines.updated(0, lines.head.drop(start.char)).updated(lines.length - 1, lines.last.take(end.char))
}
case class Pos(line: Int, char: Int) {
    override def toString: String = s"$line:$char"
}
