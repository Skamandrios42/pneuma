package pneuma.proto

import general.Pos
import general.Result
import general.Region
import scala.util.matching.Regex

class Source(content: Seq[String], index: Int) {
    val text = content.mkString("\n").drop(index)
    val pos = 
        var i = index
        var line = 0
        var char = 0
        for s <- content do
            if s.length < i then 
                i -= s.length
                line += 1
            else char = i
        Pos(line, char)

    def parseStr(s: String): Result[ParseError, (Source, Region, String)] =
        if text.startsWith(s) then 
            val newSource = Source(content, index + s.length())
            Result.succeed(newSource, Region(None, pos, newSource.pos), s)
            else (Result.fail(ParseError(s, text.take(10), Region(None, pos, pos))))
    def parseRegex(r: Regex): Result[ParseError, (Source, Region, String)] =
        r.findPrefixOf(text) match
            case Some(s) => 
                val newSource = Source(content, index + s.length())
                Result.succeed(newSource, Region(None, pos, newSource.pos), s)
            case None => (Result.fail(ParseError(r.toString, text.take(10), Region(None, pos, pos))))
}