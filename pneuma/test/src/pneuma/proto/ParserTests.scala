package pneuma.proto

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Files
import java.nio.file.Path

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
        val program = PneumaParser.fromFile("program.pneuma")
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
        val program = PneumaParser.fromFile("generator.pneuma")
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
        checked.foreach(_.foreach((a, _) => Generator("Generator", a, "generated")))
    }
}
