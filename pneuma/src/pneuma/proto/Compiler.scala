package pneuma.proto

import general.Result
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try
import scala.collection.immutable.ArraySeq

object Compiler {

    def main(args: Array[String]): Unit = 
        require(args.length == 1, "exactly one argument required!")
        val fileName = args(0)
        require(fileName.endsWith(".pneuma"), "wrong file extension!")
        val source = "{" ++ Files.readString(Path.of(fileName)) ++ "}.main"
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