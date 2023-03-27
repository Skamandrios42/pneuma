package pneuma.proto

import org.scalatest.funsuite.AnyFunSuite
import general.*
import scala.collection.immutable.ArraySeq

class PositionTests extends AnyFunSuite {
    val source = "This is some text\nAnd this is the next line\nAnd finally this is the last line!!!"
    val content = ArraySeq.unsafeWrapArray(source.split('\n'))
    for index <- 0 until source.length do
        println(s"INDEX $index")
        if source(index) != '\n' then
            println(source(index))
            val pos = Pos.from(index, content)
            println(content(pos.line)(pos.char))
            println(pos)
            println()
        else println("NEWLINE\n")
}