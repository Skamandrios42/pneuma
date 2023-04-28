package pneuma.proto

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Files
import java.nio.file.Path
import general.Result
import scala.collection.immutable.ArraySeq
import general.Region

class ParserTests extends AnyFunSuite {
    test("parsing") {
        val source =
            """{ a = \x -> x, b = \x -> x, ?c = Nat } : { a : (x: Nat) => Nat, b : (x: Type) => Type, ?c : Type }"""
        val program = PneumaParser(source)
        val term = program.map(_.convert(Map.empty))
        println(source)
        println("---")
        println(program)
        println("---")
        println(term)
    }

    test("numbers") {
        val source = """{ plus = \x -> \y -> x match { y, \k -> S (this.plus k y) } } : { plus : Nat => Nat => Nat }"""
        val program = PneumaParser(source)
        val term = program.map(_.convert(Map.empty))
        println(source)
        println("---")
        println(program)
        println("---")
        println(term)
        println("---")
        println(term.map(_.flatMap(_.typeCheck)))
    }

    test("file") {
        val program = PneumaParser.fromFile("example_programs/program.pneuma")
        val term = program.map(_.convert(Map.empty))
        println(program)
        println("---")
        println(term)
        println("---")
        val checked = term.map(_.flatMap(_.typeCheck))
        println(checked)
        println("---")
        println(checked.map(_.foreach((a, b) => println(a.eval))))
    }

    test("generator") {
        val program = PneumaParser.fromFile("example_programs/generator.pneuma")
        val term = program.map(_.convert(Map.empty))
        println(program)
        println("---")
        println(term)
        println("---")
        val checked = term.map(_.flatMap(_.typeCheck))
        println(checked)
        println("---")
        checked.map(_.foreach((a, b) => a.eval))
        println("--- NOW GENERATING")
        checked.foreach(_.foreach((a, _) => Generator.bytecode("example_programs/output/Generator", a)))
    }

    test("message") {
        val source = """{ plus = \haha -> \y -> haha match { y, \k -> S (this.plus k y) } } : { plus : Type => Nat => Nat }"""
        val program = PneumaParser(source)
        val term = program.flatMap(_.convert(Map.empty))
        println(source)
        println("---")
        println(program)
        println("---")
        println(term)
        println("---")
        term.flatMap(_.typeCheck) match
            case Result.Success(value) => println(value)
            case Result.Failure(values) => 
                values.foreach(ex => println(ex.format(ArraySeq.unsafeWrapArray(source.split('\n')))))
    }

    test("genEq") {
        val one = """{ x : Nat => { a : Nat, b : Type } }"""
        val two = """{ x : A => { a : A, b : B } }"""
        for
            a <- PneumaParser(one).flatMap(_.convert(Map.empty))
            b <- PneumaParser(two).flatMap(_.convert(Map(
                "A" -> Term.Var(1, None, Region(None, 0, 0)),
                "B" -> Term.Var(0, None, Region(None, 0, 0))
                )))
        yield println(b.genEq(a, Set.empty, List(0, 1)))
    }
}
