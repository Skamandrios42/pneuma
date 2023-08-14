package pneuma.proto

import general.Result
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer

object Compiler {

    def defaultName = "default.pneuma"

    def compileToTree(fileName: String) = for
            program <- PneumaParser(PneumaParser.removeComments("{\n" ++ Files.readString(Path.of(fileName)) ++ "\n}.main"))
            term    <- program.convert(Map.empty)
            res     <- term.typeCheck
        yield res

    def compileToJvm(fileName: String) = for
            program <- PneumaParser(PneumaParser.removeComments("{\n" ++ Files.readString(Path.of(fileName)) ++ "\n}.main"))
            term    <- program.convert(Map.empty)
            res     <- term.typeCheck
        yield Generator.bytecode(fileName.dropRight(".pneuma".length), res(0))

    def main(args: Array[String]): Unit = 
        try
            require(args.length <= 1, "exactly one argument required.")
            val fileName = Try(args(0)).getOrElse(defaultName)
            require(fileName.endsWith(".pneuma"), s"wrong file extension of file $fileName.")
            require(Files.exists(Path.of(fileName)), s"file $fileName does not exist.")
            val source = PneumaParser.removeComments("{\n" ++ Files.readString(Path.of(fileName)) ++ "\n}.main")
            val result = for
                program <- PneumaParser(source)
                term    <- program.convert(Map.empty)
                res     <- term.typeCheck
            yield
                println(s"Term: ${res(0)}")
                println(s"Type: ${res(1)}")
                Generator.bytecode(fileName.dropRight(".pneuma".length), res(0))

            result match
                case Result.Success(value) => 
                    println(s"Compilation succeeded. Generated ${value.getFileName()}.")
                case Result.Failure(values) => 
                    values.foreach(ex => println(ex.format(ArraySeq.unsafeWrapArray(source.split('\n')))))
        catch case ex: Throwable =>
            println(s"[Internal Error -- ${ex.getClass().getName()}] ${ex.getMessage()}")
            ex.getStackTrace().foreach { elem =>
                println(s" - ${elem.toString()}")
            }
}