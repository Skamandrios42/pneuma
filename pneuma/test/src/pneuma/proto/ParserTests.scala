package pneuma.proto

import org.scalatest.funsuite.AnyFunSuite

class ParserTests extends AnyFunSuite {
    test("parsing") {
        val source =
            """{ a = \x -> x, b = \x -> x, ?c = Nat } : { a : (x: Nat) => Nat, b : (x: Type) => Type, ?c : Type }"""
        val program = PneumaParser(source)
        val term = program.map(PneumaParser.convert(_, Map.empty))
        println(source)
        println("---")
        println(program)
        println("---")
        println(term)
    }
}