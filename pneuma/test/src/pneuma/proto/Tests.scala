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
        // \x -> x ?
        val te = \(0 at ?)
        val ty = (* --> *) --> (* -?> *)
        // \x -> \y -> x y
        val teRes = \(\(1 at 0))
        assert((te typeCheck ty) == Right(teRes, ty))
    }

    test("modules") {
        val te = mod(
            "id" := \(0) as * --> *,
            "te" := \(0 at ?) as ((* --> *) --> (* -?> *)),
            * ?= *,
        )
        val ty = int(
            "id" ::= * --> *,
            "te" ::= (* --> *) --> (* -?> *),
            "imp$0000" ::= *,
        )
        val teRes = mod(
            "id" := \(0),
            "te" := \(\(1 at 0)),
            "imp$0000" := *,
        )
        assert((te.typeCheck) == Right(teRes, ty))
    }

    test("modules with implicits") {
        val te = mod(
            "id" := \(0),
            * ?= *,
            "use" := (0 ! "id") at ?,
            "res" := (0 ! "use"),
        )
        val ty = int(
            "id" ::= * --> *,
            *,
            "use" ::= * -?> *,
            "res" ::= *,
        )
        assert((te typeCheck ty) == Right(ty))
        println(((te as ty) ! "use") typeCheck (* -?> *))
        println(((te as ty) ! "res") typeCheck *)
    }

}