package pneuma.proto

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite
import DSL.{*, given}
import general.Result
import scala.util.Try
import general.Logger

class TermTests extends AnyFunSuite {

    given Logger = Logger.default

    extension (self: Result[TypeError, (Term, Term)]) def untag = self.map((x, y) => (x.untag.erase, y.untag.erase))

    test("miscellaneous") {
        val te = \(\(1 at 0))
        val ty = (* --> *) --> (* --> *)
        assert((te as ty).typeCheck.untag == Result.Success(te as ty, ty))
    }

    test("implicits I") {
        val te = \(0 at ?)
        val ty = (* --> *) --> (* -?> *)
        val teRes = \(\(1 at 0))
        assert((te as ty).typeCheck.untag == Result.Success(teRes as ty, ty))
    }
    //Success((\ -> \ -> (1 0))), ((Type => Type) => (Type =?> Type)))) did not equal 
    //Success((\ -> \ -> (1 0))), ((Type => Type) => (Type =?> Type))))

    test("implicits II") {
        val te = \(mod("x" := 1, "y" := ?))
        val ty = Nat --> (Nat -?> int("x" ::= Nat, "y" ::= Nat))
        val teRes = \(\(mod("x" := 2, "y" := 1)))
        assert((te as ty).typeCheck.untag == Result.Success(teRes as ty, ty))
    }

    test("implicits III") {
        val typeWithInstance = int("A" ::= *, "a" ::= (0 ! "A"))
        val te = \(0 ! "a")
        val ty = typeWithInstance --> (Nat -?> (1 ! "A"))
        val teRes = \(\(1 ! "a"))
        assert((te as ty).typeCheck.untag == Result.Success(teRes as ty, ty))
    }

    test("implicits IV") {
        val program = """{ 
            ?a : { x : Nat } = { x = 42 }, 
            summon : (A : Type) => A =?> A = \A -> ?, 
            main : Nat = (summon { x : Nat }).x
        }.main"""
        val typ = PneumaParser(program).flatMap(_.convert(Map.empty)).flatMap(_.typeCheck).map(_(1))
        assert(typ.map(_.erase) == Result.Success(Nat))
    }

    test("implicits V") {
        val program = """{ 
            ?a : Nat => Nat = \x -> S x, 
            summon : (A : Type) => A =?> A = \A -> ?, 
            main : Nat = summon (Nat => Nat) 42
        }.main"""
        val typ = PneumaParser(program).flatMap(_.convert(Map.empty)).flatMap(_.typeCheck).map(_(1))
        assert(typ.map(_.erase) == Result.Success(Nat))
    }

    test("modules") {
        val te = mod(
            "id" := \(0),
            "te" := \(0 at ?),
            "imp$0000" :?= *,
        )
        val ty = int(
            "id" ::= * --> *,
            "te" ::= (* --> *) --> (* -?> *),
            "imp$0000" ::?= *,
        )
        val teRes = mod(
            "id" := \(0),
            "te" := \(\(1 at 0)),
            "imp$0000" :?= *,
        )
        assert(((te as ty).typeCheck).untag == Result.Success(teRes as ty, ty))
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
        assert((te as ty).typeCheck.untag == Result.Success(teRes as ty, ty))
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
        assert((te as ty).typeCheck.untag == Result.Success(teRes as ty, ty))
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
        assert((Example.te typeCheck Example.ty).untag == Result.Success(Example.te, Example.ty))
    }

    test("implicits in modules III b") {
        val test = \(mod("x" := (1 ! "imp$1") at ?))
        val tyst = Example.ty --> int("x" ::= ((1 ! "A") -?> (2 ! "B")))
        val expected = \(mod("x" := \((2 ! "imp$1") at 0)))
        assert((test typeCheck tyst).untag == Result.Success(expected, tyst))
    }

    test("implicits in modules III c") {
        val test = \(mod(
            "imp$0" :?= (1 ! "a"),
            "imp$1" :?= (1 ! "imp$1") at ?,
            "imp$2" :?= (1 ! "imp$2") at ?,
            "res" := ?
        ))
        val interface = Example.ty --> int(
            "imp$0" ::?= (1 ! "A"),
            "imp$1" ::?= ((1 ! "A") -?> (2 ! "B")),
            "imp$2" ::?= ((1 ! "B") -?> (2 ! "C")),
            "res" ::= (1 ! "C")
        )
        val expected = \(mod(
            "imp$0" :?= (1 ! "a"),
            "imp$1" :?= \((2 ! "imp$1") at 0),
            "imp$2" :?= \((2 ! "imp$2") at 0),
            "res" := (0 ! "imp$2") at ((0 ! "imp$1") at (0 ! "imp$0"))
        ))

        assert((test typeCheck interface).untag == Result.Success(expected, interface))
    }

    test("implicits in modules IV") {
        val te = mod(
            "a" :?= Nat,
            "b" := nat(42)
        )
        val ty = int(
            "a" ::?= *,
            "b" ::= ?,
        )
        val teExp = mod(
            "a" :?= Nat,
            "b" := nat(42)
        )
        val tyExp = int(
            "a" ::?= *,
            "b" ::= (0 ! "a"),
        )

        assert((te as ty).typeCheck.untag == Result.Success(teExp as tyExp, tyExp))
    }

    test("implicits in modules V") {
        val te = mod(
            "a" :?= Nat,
            "b" := nat(42),
            "c" := (0 ! "b")
        )
        val ty = int(
            "a" ::?= *,
            "b" ::= ?,
            "c" ::= Nat
        )
        val teExp = mod(
            "a" :?= Nat,
            "b" := nat(42),
            "c" := (0 ! "b")
        )
        val tyExp = int(
            "a" ::?= *,
            "b" ::= (0 ! "a"),
            "c" ::= Nat
        )

        assert((te as ty).typeCheck.untag == Result.Success(teExp as tyExp, tyExp))
    }

    test("this has to work") {
        val te = \(\(0))
        val ty = * --> (0 --> 1)
        assert((te as ty).typeCheck.untag == Result.Success(te as ty, ty))
    }

    test("shifting in module projection") {

        println(PneumaParser("""( \A -> \a -> { x = a, y = this.x } ) : ((A : Type) => A => { x : A, y : A })""").flatMap(_.convert(Map.empty)).flatMap(_.typeCheck))
        println(PneumaParser("""( \A -> \a -> { x = a } ) : ((A : Type) => A => { x : A })""").flatMap(_.convert(Map.empty)).flatMap(_.typeCheck))



        val te = \(\(mod(
            "x" := 1,
            "y" := (0 ! "x"),
        )))
        val ty = * --> (0 --> int(
            "x" ::= 2,
            "y" ::= 2,
        ))
        assert((te as ty).typeCheck.untag == Result.Success(te as ty, ty))
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
        assert(te.typeCheck.untag == Result.Success(te, ty))
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
        assert((te as ty).typeCheck.untag == Result.Success(te as ty, ty))
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
        assert((te as ty).typeCheck.untag == Result.Success(te as ty, ty))
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
        assert((te as ty).typeCheck.untag == Result.Success(te as ty, ty))
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
        assert((te as ty).typeCheck.untag == Result.Success(te as ty, ty))
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

    test("eq") {
        try {
            // { x = { x = { x = ... } 0 } 0 } 0
            // { x = this 1 }
            val m1 = mod("x" := 0 at 1)
            val m2 = mod("x" := 0 at 2)
            println((mod("x" := 0 at 1) at 1) === (mod("x" := 0 at 2) at 2))
        } catch case ex => ()
    }
}
