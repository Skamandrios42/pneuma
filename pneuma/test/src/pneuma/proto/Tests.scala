package pneuma.proto

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite
import DSL.{*, given}

class Tests extends AnyFunSuite {

    extension (self: Either[String, (Term, Term)]) def untag = self.map((x, y) => (x.untag, y.untag))

    test("miscellaneous") {
        val te = \(\(1 at 0))
        val ty = (* --> *) --> (* --> *)
        assert((te typeCheck ty) == Right(te, ty))
    }

    test("implicits I") {
        val te = \(0 at ?)
        val ty = (* --> *) --> (* -?> *)
        val teRes = \(\(1 at 0))
        assert((te typeCheck ty) == Right(teRes, ty))
    }

    test("implicits II") {
        val te = \(mod("x" := 1, "y" := ?))
        val ty = Nat --> (Nat -?> int("x" ::= Nat, "y" ::= Nat))
        val teRes = \(\(mod("x" := 2, "y" := 1)))
        assert((te typeCheck ty).untag == Right(teRes, ty))
    }

    test("implicits III") {
        val typeWithInstance = int("A" ::= *, "a" ::= (0 ! "A"))
        val te = \(0 ! "a")
        val ty = typeWithInstance --> (Nat -?> (1 ! "A"))
        val teRes = \(\(1 ! "a"))
        assert((te typeCheck ty).untag == Right(teRes, ty))
    }

    test("modules") {
        val te = mod(
            "id" := \(0),
            "te" := \(0 at ?),
            "imp$0000" ?: * ?= *,
        )
        val ty = int(
            "id" ::= * --> *,
            "te" ::= (* --> *) --> (* -?> *),
            "imp$0000" ?= *,
        )
        val teRes = mod(
            "id" := \(0),
            "te" := \(\(1 at 0)),
            "imp$0000" := *,
        )
        assert((te.typeCheck(ty)).untag == Right(teRes, ty))
    }

    test("implicits in modules I") {
        val te = mod(
            "a" := \(0),
            "c" := (0 ! "a") at ?
        )
        val ty = int(
            "a" ::= * --> *,
            "c" ::= * -?> *
        )
        val teRes = mod(
            "a" := \(0),
            "c" := \((1 ! "a") at 0)
        )
        assert((te typeCheck ty).untag == Right(teRes, ty))
    }

    test("implicits in modules II") {
        val te = mod(
            "a" := \(0),
            "c" := (0 ! "a") at ?
        )
        val ty = * -?> int(
            "a" ::= * --> *,
            "c" ::= *
        )
        val teRes = \(mod(
            "a" := \(0),
            "c" := (0 ! "a") at 1
        ))
        assert((te typeCheck ty).untag == Right(teRes, ty))
    }

    object Example {
        val te = mod(
            "A" := Nat,
            "B" := Nat,
            "C" := Nat,
            "a" := nat(42),
            "imp$1" := \(0.succ),
            "imp$2" := \(0.succ),
        )
        val ty = int(
            "A" ::= *,
            "B" ::= *,
            "C" ::= *,
            "a" ::= (0 ! "A"),
            "imp$1" ::= (0 ! "A") --> (1 ! "B"),
            "imp$2" ::= (0 ! "B") --> (1 ! "C"),
        )
    }

    test("implicits in modules III a") {
        assert((Example.te typeCheck Example.ty).untag == Right(Example.te, Example.ty))
    }

    test("implicits in modules III b") {
        val test = \(mod("x" := (1 ! "imp$1") at ?))
        val tyst = Example.ty --> int("x" ::= ((1 ! "A") -?> (2 ! "B")))
        val expected = \(mod("x" := \((2 ! "imp$1") at 0)))
        assert((test typeCheck tyst).untag == Right(expected, tyst))
    }

    test("implicits in modules III c") {
        val test = \(mod(
            "imp$0" ?: (1 ! "A") ?= (1 ! "a"),
            "imp$1" ?: ((1 ! "A") -?> (2 ! "B")) ?= (1 ! "imp$1") at ?,
            "imp$2" ?: ((1 ! "B") -?> (2 ! "C")) ?= (1 ! "imp$2") at ?,
            "res" := ?
        ))
        val interface = Example.ty --> int(
            "imp$0" ?= (1 ! "A"),
            "imp$1" ?= ((1 ! "A") -?> (2 ! "B")),
            "imp$2" ?= ((1 ! "B") -?> (2 ! "C")),
            "res" ::= (1 ! "C")
        )
        val expected = \(mod(
            "imp$0" := (1 ! "a"),
            "imp$1" := \((2 ! "imp$1") at 0),
            "imp$2" := \((2 ! "imp$2") at 0),
            "res" := (0 ! "imp$2") at ((0 ! "imp$1") at (0 ! "imp$0"))
        ))

        assert((test typeCheck interface).untag == Right(expected, interface))
    }

    test("implicits in modules IV") {
        val te = mod(
            "a" ?: * ?= Nat,
            "b" := nat(42)
        )
        val ty = int(
            "a" ?= *,
            "b" ::= ?,
        )
        val teExp = mod(
            "a" := Nat,
            "b" := nat(42)
        )
        val tyExp = int(
            "a" ?= *,
            "b" ::= (0 ! "a"),
        )

        assert(te.typeCheck(ty).untag == Right(teExp, tyExp))
    }

    test("implicits in modules V") {
        val te = mod(
            "a" ?: * ?= Nat,
            "b" := (0 ! "c"),
            "c" := nat(42)
        )
        val ty = int(
            "a" ?= *,
            "b" ::= ?,
            "c" ::= Nat
        )
        val teExp = mod(
            "a" := Nat,
            "b" := (0 ! "c"),
            "c" := nat(42)
        )
        val tyExp = int(
            "a" ?= *,
            "b" ::= (0 ! "a"),
            "c" ::= Nat
        )

        assert(te.typeCheck(ty).untag == Right(teExp, tyExp))
    }

    test("shifting in module projection") {
        val te = \(\(mod(
            "x" := 1,
            "y" := (0 ! "x"),
        )))
        val ty = * --> (0 --> int(
            "x" ::= (\(0) as (* --> *)) at 2,
            "y" ::= 2,
        ))
        assert(te.typeCheck(ty).untag == Right(te, ty))
    }

    test("ascriptions") {
        val te = mod(
            "A" := Nat,
            "a" := nat(0) as (0 ! "A")
        )
        val ty = int(
            "A" ::= *, 
            "a" ::= (0 ! "A")
        )
        println(te.typeCheck.untag)
        assert(te.typeCheck.untag == Right(te, ty))
    }

    test("type operators I") {
        val te = mod(
            "op" := \(0),
            "a" := 0 ! "op"

        )
        val ty = int(
            "op" ::= * --> *,
            "a" ::= (0 ! "op") at (* --> *)
        )
        assert((te typeCheck ty).untag == Right(te, ty))
    }

    test("type operators II") {
        val te = mod(
            "op" := \(* --> 1),
            "a" := 0 ! "op"
        )
        val ty = int(
            "op" ::= * --> *,
            "a" ::= (0 ! "op") at * 
        )
        assert((te typeCheck ty).untag == Right(te, ty))
    }

    test("recursion") {
        val te = mod(
            "a" := 0 ! "b",
            "b" := 0 ! "a",
            "c" := 0 ! "c"
        )
        val ty = int(
            "a" ::= *,
            "b" ::= *,
            "c" ::= *
        )
        assert((te typeCheck ty).untag == Right(te, ty))
    }

    test("naturals") {
        val te = mod(
            "sum" := \(\(1.pattern(onZero = 0, onSucc = (3 ! "sum" at 0 at 1).succ))),
            "times" := \(\(1.pattern(onZero = nat(0), onSucc = (3 ! "sum") at 1 at (3 ! "times" at 0 at 1)))),
            "fact" := \(0.pattern(onZero = nat(1), onSucc = (2 ! "times") at 1 at ((2 ! "fact") at 0))),
            "b" := (0 ! "sum") at nat(42) at nat(48),
            "c" := nat(42).succ
        )
        val ty = int(
            "sum" ::= Nat --> (Nat --> Nat),
            "times" ::= Nat --> (Nat --> Nat),
            "fact" ::= Nat --> Nat,
            "b" ::= Nat,
            "c" ::= Nat
        )
        assert((te typeCheck ty).untag == Right(te, ty))
        val tasks = List(
            (te ! "sum" at nat(0) at nat(0), nat(0)),
            (te ! "sum" at nat(1) at nat(0), nat(1)),
            (te ! "sum" at nat(0) at nat(1), nat(1)),
            (te ! "sum" at nat(3) at nat(5), nat(8)),
            (te ! "times" at nat(0) at nat(10), nat(0)),
            (te ! "times" at nat(10) at nat(0), nat(0)),
            (te ! "times" at nat(1) at nat(5), nat(5)),
            (te ! "times" at nat(5) at nat(1), nat(5)),
            (te ! "times" at nat(4) at nat(3), nat(12)),
            (te ! "fact" at nat(0), nat(1)),
            (te ! "fact" at nat(1), nat(1)),
            (te ! "fact" at nat(2), nat(2)),
            (te ! "fact" at nat(3), nat(6)),
            (te ! "fact" at nat(4), nat(24)),
        )
        tasks.foreach {
            (a, b) => 
                val res = a.eval
                println(res)
                assert(res == b)
        }
    }

}
