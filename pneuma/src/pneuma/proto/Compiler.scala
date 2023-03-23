package pneuma.proto

import general.Result

object Compiler {

    def main(args: Array[String]) = 
        require(args.length == 1, "exactly one argument required!")
        val fileName = args(0)
        require(fileName.endsWith(".pneuma"), "wrong file extension!")
        val done = for 
            program <- PneumaParser.fromFile(fileName)
            term    <- program.convert(Map.empty)
            res     <- term.typeCheck
        yield Generator(fileName.dropRight(".pneuma".length), res(0), ".")

        done match
            case Result.Success(value) =>
            case Result.Failure(values) => values.foreach(println)
}