package pneuma.proto

import general.Pos
import general.Result
import general.Metadata
import scala.util.matching.Regex
import scala.collection.immutable.ArraySeq

class Source(val text: String, val index: Int) {
    //def lines: Seq[String] = ArraySeq.unsafeWrapArray(text.split('\n'))
    //def pos = Pos.from(index, lines)

    def parseStr(s: String, file: Option[String]): Result[ParseError, (Source, Metadata, String)] =
        if text.drop(index).startsWith(s) then 
            val newSource = Source(text, index + s.length)
            Result.succeed(newSource, Metadata(None, index, newSource.index), s)
            else (Result.fail(ParseError(s, text.drop(index).take(10), Metadata(file, index, index))))
    def parseRegex(r: Regex, file: Option[String]): Result[ParseError, (Source, Metadata, String)] =
        r.findPrefixOf(text.drop(index)) match
            case Some(s) =>
                val newSource = Source(text, index + s.length())
                Result.succeed(newSource, Metadata(None, index, newSource.index), s)
            case None => (Result.fail(ParseError(r.toString, text.drop(index).take(10), Metadata(file, index, index))))
}