package pneuma.proto

import general.Result
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import general.Region
import general.Pos

object Compiler {

    def defaultName = "default.pneuma"

    def main(args: Array[String]): Unit = 
        require(args.length <= 1, "exactly one argument required!")
        val fileName = Try(args(0)).getOrElse(defaultName)
        require(fileName.endsWith(".pneuma"), "wrong file extension!")
        val source = "{\n" ++ Files.readString(Path.of(fileName)) ++ "\n}.main"
        val done = for
            program <- PneumaParser(source)
            term    <- program.convert(Map.empty)
            res     <- term.typeCheck
        yield Generator(fileName.dropRight(".pneuma".length), res(0))

        done match
            case Result.Success(value) => 
                println(s"Compilation succeeded. Generated ${value.getFileName()}.")
            case Result.Failure(values) => 
                values.foreach(ex => println(ex.format(ArraySeq.unsafeWrapArray(source.split('\n')))))
}