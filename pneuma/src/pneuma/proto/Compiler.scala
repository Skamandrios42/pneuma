package pneuma.proto

import general.Result
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import general.Logger
import general.Logger.Level

object Compiler {

    def defaultName = "default.pneuma"
    given Logger = Logger.default.set(Level.Trace)

    def main(args: Array[String]): Unit = 
        try
            require(args.length <= 1, "exactly one argument required.")
            val fileName = Try(args(0)).getOrElse(defaultName)
            require(fileName.endsWith(".pneuma"), s"wrong file extension of file $fileName.")
            require(Files.exists(Path.of(fileName)), s"file $fileName does not exist.")
            val source = PneumaParser.removeComments("{\n" ++ Files.readString(Path.of(fileName)) ++ "\n}.main")
            Logger.log(s"Compiling $fileName", Level.Info)
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
                    Logger.log(s"Compilation succeeded. Generated ${value.getFileName()}", Level.Info)
                case Result.Failure(values) => 
                    values.foreach(ex => println(ex.format(ArraySeq.unsafeWrapArray(source.split('\n')))))
        catch 
            case ex: IllegalArgumentException =>
                Logger.log(s"${ex.getClass().getName()}: ${ex.getMessage()}", Level.Error)
                println(ex.getMessage())
            case ex: Throwable =>
                Logger.log(s"${ex.getClass().getName()}: ${ex.getMessage()}", Level.Fatal)
                println(s"[Internal Error -- ${ex.getClass().getName()}] ${ex.getMessage()}")
                ex.getStackTrace().foreach { elem =>
                    println(s" - ${elem.toString()}")
                }
}