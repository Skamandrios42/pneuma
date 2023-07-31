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

    test("scalaBug") {
        import Term.*

        extension (self: Term) {
        def genBug(that: Term, relation: Term#R, variables: List[Int]): Option[List[(Int, Term)]] =
            println(s"genBug [$self] === [$that]  ---- ${self.getClass} / ${that.getClass}")
            import self.{>>, &&, <<}

            if relation(self, that) then Some(Nil) else
                // relation with assumed equivalence of self and right
                def updatedRelation = relation + ((self, that))
                (self.eval, that.eval) match
                    case (Var(x, _, _), term) => 
                        println(s"VAR $x ---> $term")
                        Option.when(variables contains x)((x, term) :: Nil)
                    case (Abs(t, _, _), Abs(e, _, _)) => 
                        println("ABS")
                        t.genBug(e, updatedRelation, variables >> 1) << 1
                    case (App(t1, t2, _), App(e1, e2, _)) =>
                        println("APP")
                        t1.genBug(e1, updatedRelation, variables) && t2.genBug(e2, updatedRelation, variables)
                    case (Typ(_), Typ(_)) => 
                        println("TYP")
                        Some(Nil)
                    case (Phi(_), Phi(_)) => throw new Exception("equivalence should only be called after implicit resolution")
                    case (Pro(t1, t2, _, _), Pro(e1, e2, _, _)) => 
                        println("PRO")
                        t1.genBug(e1, updatedRelation, variables) && (t2.genBug(e2, updatedRelation, variables >> 1) << 1)
                    case (Imp(t1, t2, _), Imp(e1, e2, _)) => 
                        println("IMP")
                        t1.genBug(e1, updatedRelation, variables) && (t2.genBug(e2, updatedRelation, variables >> 1) << 1)
                    case (Module(ts, _), Module(es, _)) => 
                        println("MOD")
                        (ts zip es).foldLeft(Option(List.empty[(Int, Term)])) {
                            case (opt, (ModElem(s1, t1, m1), ModElem(s2, t2, m2))) => 
                                opt.flatMap { xs => 
                                    (t1.genBug(t2, updatedRelation, variables >> 1) << 1).flatMap { ys =>
                                        Option.when(s1 == s2 && m1 == m2)(xs ++ ys)
                                    }
                                }
                        }
                    case (Interface(ts, _), Interface(es, _)) => 
                        println("INT")
                        (ts zip es).foldLeft(Option(List.empty[(Int, Term)])) {
                        case (opt, (IntElem(s1, t1, m1), IntElem(s2, t2, m2))) => 
                                opt.flatMap { xs => 
                                    (t1.genBug(t2, updatedRelation, variables >> 1) << 1).flatMap { ys =>
                                        Option.when(s1 == s2 && m1 == m2)(xs ++ ys)
                                    }
                                }
                        }
                    case (As(t1, t2, _), As(e1, e2, _)) => println("AS")
                        t1.genBug(e1, updatedRelation, variables) && t2.genBug(e2, updatedRelation, variables)
                    case (Get(t, f1, _), Get(e, f2, _)) => println("GET")
                        t.genBug(e, updatedRelation, variables).filter(_ => f1 == f2)
                    case (NatType(_), NatType(_)) => println("NATTYPE")
                        Some(Nil)
                    case (Nat(v1, _), Nat(v2, _)) => println("NAT")
                        Option.when(v1 == v2)(Nil)
                    case (Succ(t1, _), Succ(t2, _)) => println("SUC")
                        t1.genBug(t2, updatedRelation, variables)
                    case (Debug(t1, _), Debug(t2, _)) => println("DEB")
                        t1.genBug(t2, updatedRelation, variables)
                    case (Match(t1, z1, s1, _, _), Match(t2, z2, s2, _, _)) => println("MAT")
                        t1.genBug(t2, updatedRelation, variables) && z1.genBug(z2, updatedRelation, variables) && (s1.genBug(s2, updatedRelation, variables >> 1) << 1)
                    case _ => println("NONE")
                        None
        }
        // genBug(App())
    }

    // test("inferAll") {
    //     val one = """[A : Type] => [B : Type] => A => B => A"""
    //     val two = """Nat => Type => Nat"""
    //     for
    //         a <- PneumaParser(one).flatMap(_.convert(Map.empty))
    //         b <- PneumaParser(two).flatMap(_.convert(Map.empty))
    //     yield println(Term.Typ(Region(None, 0, 0)).inferAll(a, b, Map.empty))
    // }

    // test("inferPartial") {
    //     import Term.*

    //     val one = """[A : Type] => [B : Type] => [C : Type] => A => B => A"""
    //     val two = """[A : Type] => [B : Type] => [C : Type] => B => B => C"""
    //     val thr = """[A : Type] => [B : Type] => [C : Type] => C => B => A"""
    //     for
    //         a <- PneumaParser(one).flatMap(_.convert(Map.empty))
    //         b <- PneumaParser(two).flatMap(_.convert(Map.empty))
    //         c <- PneumaParser(thr).flatMap(_.convert(Map.empty))
    //     yield 
    //         println(a.inferPartialForApp(a, Term.Var(0, None, Region.none), Map.empty).map(_.revert(Map(0 -> "X"))))
    //         println(b.inferPartialForApp(b, Term.Var(0, None, Region.none), Map.empty).map(_.revert(Map(0 -> "X"))))
    //         println(c.inferPartialForApp(c, Term.Var(0, None, Region.none), Map.empty).map(_.revert(Map(0 -> "X"))))
    // }
}
