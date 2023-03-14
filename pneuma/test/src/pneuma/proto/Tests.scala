package pneuma.proto

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite
import DSL.{*, given}

class Tests extends AnyFunSuite {

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
            * ?= *,
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
        assert((te.typeCheck(ty)) == Right(teRes, ty))
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
        assert((te typeCheck ty).map((x, y) => (x.untag, y.untag)) == Right(teRes, ty))
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
        assert((te typeCheck ty).map((x, y) => (x.untag, y.untag)) == Right(teRes, ty))
    }

}
