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

    test("implicits") {
        val te = \(0 at ?)
        val ty = (* --> *) --> (* -?> *)
        val teRes = \(\(1 at 0))
        assert((te typeCheck ty) == Right(teRes, ty))
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

    test("type operators I") {
        val te = mod(
            "op" := \(0),       // \x.x
            "a" := 0 ! "op"     // this.op ~~> \x.x

        )
        val ty = int(
            "op" ::= * --> *,                   // * -> *
            "a" ::= (0 ! "op") at (* --> *)     // this.op (* -> *)
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
            "sum" := \(\(Term.Match(1,
                onZero = 0, onSucc = Term.Succ(3 ! "sum" at 0 at 1)
            ))),
            "b" := (0 ! "sum") at Term.Nat(42) at Term.Nat(48),
            "c" := Term.Succ(Term.Nat(42))
        )
        val ty = int(
            "sum" ::= Term.NatType --> (Term.NatType --> Term.NatType),
            "b" ::= Term.NatType,
            "c" ::= Term.NatType
        )
        assert((te typeCheck ty).untag == Right(te, ty))
        println((te ! "sum"))
        println((te ! "b").eval)
    }

}
