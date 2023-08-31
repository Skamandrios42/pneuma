package pneuma.proto

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite
import general.Result
import general.Metadata
import general.Logger
import general.Logger.Level

trait MyTests {
    extension (sc: StringContext) def pneuma(args: Any*): Term = PneumaParser(sc.raw(args*)).get.convert(Map.empty).get.erase
    extension (str: String) def pneuma: Term = PneumaParser(str).get.convert(Map.empty).get.erase
    export DSL.{*, given}
}

class SystematicTests extends AnyFunSuite with MyTests {

    given Logger = Logger.default.set(Level.Trace)

    test("search()") {
        val m = int(
            "A" ::= *,
            "B" ::= *,
            "C" ::= *,
            "a" ::= 0.get("A"),
            "f" ::= (0.get("A") --> 1.get("B")),
            "g" ::= (0.get("B") --> 1.get("C")),
        )
        val map = Map(
            m.get("A") -> m.get("a"), 
            (m.get("A") -?> m.get("B")) -> m.get("f"),
            (m.get("B") -?> m.get("C")) -> m.get("g")
        )

        assert(*.search(Nat, Map(Nat -> nat(42))) == Some(nat(42)))
        assert(*.search(m.get("C"), map) == Some(m.get("g").at(m.get("f").at(m.get("a")))))
    }
    test("requirements()") {
        val t0 = * --> (1 at 0)
        val t1 = * -#> (0 -?> (1 --> (0 -?> (1 --> (1 at 0)))))
        assert(t0.requirements == (Nil, t0))
        assert(t1.requirements == (List(Term.Req.Inf(*), Term.Req.Imp(0)), (1 --> (0 -?> (1 --> (1 at 0))))))
    }
    test("independentOf()") {
        val t = 0 at (\(1) at \(\(5)))
        assert( t.independentOf(1, 2))
        assert(!t.independentOf(1, 3))
        assert( t.independentOf(-5, -1))
        assert( t.independentOf(4, 7))
    }
    test("genEq() - 1") {
        val (xs, t) = PneumaParser("""[y : Type] => (x : Type => y) => x y""").get.convert(Map.empty).get.erase.requirements
        val res = t.genEq(PneumaParser("""(x : Type => Nat) => x Nat""").get.convert(Map.empty).get.erase, Set.empty, List(0))
        assert(xs == List(Term.Req.Inf(*)))
        assert(res == Some(List(0 -> Nat, 0 -> Nat)))
    }
    test("genEq() - 2") {
        val (xs, t) = PneumaParser("""[a : Type] => [b : Type] => a => b => a""").get.convert(Map.empty).get.erase.requirements
        val res = t.genEq(PneumaParser("""Nat => Type => (Type => Nat)""").get.convert(Map.empty).get.erase, Set.empty, List(0, 1))
        assert(xs == List(Term.Req.Inf(*), Term.Req.Inf(*)))
        assert(res == Some(List(1 -> Nat, 0 -> *, 1 -> (* --> Nat))))
    }
    test("validateGenEqRes()") {
        val xs = List(0 -> *, 1 -> Nat, 0 -> *, 2 -> (* --> *), 1 -> Nat, 2 -> (\(0 --> *).at(*)))
        assert(*.validateGenEqRes(xs) == Some(Map(0 -> *, 1 -> Nat, 2 -> (* --> *))))
        assert(*.validateGenEqRes(2 -> (\(0 --> *).at(Nat)) :: xs) == None)
    }
    test("unify()") {
        val map1 = Map(0 -> *, 1 -> Nat, 2 -> (* --> *))
        val map2 = Map(0 -> *, 2 -> \(0 --> 1).at(*), 3 -> *)
        assert(*.unify(map1, map1) == Result.succeed(map1))
        assert(*.unify(map2, map2) == Result.succeed(map2))
        assert(*.unify(map1, map2) == Result.succeed(map2 ++ map1))
    }
    test("resolve()") {
        val t0 = pneuma"[A : Type] => [a : A] => A a"
        val u0 = pneuma"Nat 3"
        val (xs0, c0) = t0.requirements
        assert(*.resolve(xs0, c0, u0, Map.empty, Map.empty, Map.empty) == Result.succeed(Map(0 -> nat(3), 1 -> Nat)))

        val t1 = pneuma"[A : Type] => [a : A] => A a"
        val u1 = pneuma"Nat Nat"
        val (xs1, c1) = t1.requirements
        assert(*.resolve(xs1, c1, u1, Map.empty, Map.empty, Map.empty) == Result.fail(TypeError.Message(s"constraints cannot be unified Type and Nat", Metadata.none)))

        val t2 = pneuma"[A : Type] => [a : A] => A a"
        val u2 = 0.at(1)
        val (xs2, c2) = t2.requirements
        assert(*.resolve(xs2, c2, u2, Map(0 -> *, 1 -> 0), Map.empty, Map.empty) == Result.succeed[Map[Int, Term]](Map(0 -> 3, 1 -> 2)))
    }

    test("reconstruct()"):
        val ty0 = "[A : Type] => [a : A] => A a".pneuma
        assert(*.reconstruct(?, ty0, 1, Map(1 -> Nat, 0 -> nat(3)), Map()) { case (te, other) => Result.succeed(te, other) }.get == ("?".pneuma, "Nat 3".pneuma))
        assert(*.reconstruct(?, ty0, 1, Map(1 -> Nat), Map()) { case (te, other) => Result.succeed(te, other) }.get == ("?".pneuma, "[a : Nat] => Nat a".pneuma))
        assert(*.reconstruct(?, ty0, 1, Map(1 -> 2, 0 -> 3), Map()) { case (te, other) => Result.succeed(te, other) }.get == (?, 0 at 1))

    test("resolveAll() - 0") {
        val ty = pneuma"[A : Type] => A =?> [a : A] => A a"
        println(ty.requirements)
        val sh = pneuma"Nat 3"
        assert(*.resolveAll(*, ty, sh, Map.empty, Map(Nat -> nat(42)), Map.empty) == Result.succeed(pneuma"Type 42", pneuma"Nat 3"))
    }
    test("resolveAll() - 1") {
        val ty = "[A : Type] => [a : A] => A a".pneuma
        println(ty.requirements)
        val sh = (0 at 1)
        assert(*.resolveAll(*, ty, sh, Map(0 -> *, 1 -> 0), Map.empty, Map.empty) == Result.succeed(*, sh))
    }
    test("resolveApp()") {
        val ty = "[A : Type] => [a : A] => a => a".pneuma
        val sh = (0 at 1)
        assert(*.resolveApp(?, nat(42), ty, Map.empty, Map.empty, Map.empty).log == Result.succeed(? at nat(42), Nat))

        val ty0 = "[A : Type] => [B : Type] => A => { a: A, b: B } ".pneuma
        //assert(*.resolveApp(?, nat(42), ty0, Map.empty, Map.empty, Map.empty).log == Result.succeed(? at nat(42), "[B : Type] => { a: Nat, b: B }".pneuma))
        val ty1 = "[A : Type] => [B : Type] => B => { a: A, b: B } ".pneuma
        //assert(*.resolveApp(?, nat(42), ty1, Map.empty, Map.empty, Map.empty).log == Result.succeed(? at nat(42), "[A : Type] => { a: A, b: Nat }".pneuma))
    }

}