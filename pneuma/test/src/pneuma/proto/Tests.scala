package pneuma.proto

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite
import DSL.{*, given}

class Tests extends AnyFunSuite {

    test("miscellaneous") {
        val te = \(\(1 at 0))
        val ty = (* --> *) --> (* --> *)
        assert((te typeCheck ty) == Right(ty))
    }

    test("implicits") {
        val te = \(0 at ?)
        val ty = (* --> *) --> (* -?> *)
        assert((te typeCheck ty).getOrElse(?) == ty)
    }

    test("modules") {
        val te = mod(
            "id" := \(0),
            "use" := (0 ! "id") at ?,
            "res" := (0 ! "use"),
        )(
            * -> *
        )
        val ty = int(
            "id" := * --> *,
            "use" := * -?> *,
            "res" := *,
        )(
            *
        )
        assert((te typeCheck ty) == Right(ty))
        println(((te as ty) ! "use") typeCheck (* -?> *))
        println(((te as ty) ! "res") typeCheck *)
    }

}