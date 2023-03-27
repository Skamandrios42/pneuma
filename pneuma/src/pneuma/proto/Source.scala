package pneuma.proto

import general.Pos
import general.Result
import general.Region
import scala.util.matching.Regex
import scala.collection.immutable.ArraySeq

class Source(val text: String, val index: Int) {
    //def lines: Seq[String] = ArraySeq.unsafeWrapArray(text.split('\n'))
    //def pos = Pos.from(index, lines)

    def parseStr(s: String, file: Option[String]): Result[ParseError, (Source, Region, String)] =
        if text.drop(index).startsWith(s) then 
            val newSource = Source(text, index + s.length)
            Result.succeed(newSource, Region(None, index, newSource.index), s)
            else (Result.fail(ParseError(s, text.drop(index).take(10), Region(file, index, index))))
    def parseRegex(r: Regex, file: Option[String]): Result[ParseError, (Source, Region, String)] =
        r.findPrefixOf(text.drop(index)) match
            case Some(s) =>
                val newSource = Source(text, index + s.length())
                Result.succeed(newSource, Region(None, index, newSource.index), s)
            case None => (Result.fail(ParseError(r.toString, text.take(10).drop(index), Region(file, index, index))))
}