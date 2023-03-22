import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill._
import scalalib._
import mill.contrib.scoverage.ScoverageModule

/* version 0.0.0 -- core 1 */
// TODO module checking with expected shape DONE
// TODO module insertion preventation -- better module checking algorithm DONE
// TODO equivalence function DONE

/* version 0.0.0 -- core 2 */
// TODO document pneuma.proto DONE
// TODO make modules recursive DONE
// TODO rethink the implicit naming conventions DONE
// TODO introduce natural numbers DONE

// TODO exact semantics of Ascriptions DONE?
// TODO need to tag implicit context DONE?
// TODO what happens with implicits in types DONE?
// TODO recheck complete structure DONE
// TODO testing DONE

/* version 0.0.0 -- infrastructure */
// TODO build a parser DONE
// TODO build a bytecode generator
// TODO testing
// TODO make base types

/* version 0.0.0 -- release */
// TODO initialize repository DONE
// TODO make publishing setup
// TODO clean project
// TODO use ZIO for speedup?

/* version 0.1.0 */
// TODO inductive datatypes
// TODO typeclasses for literal macros
// TODO mutual recursive modules DONE? i think so

// POTENTIAL BUGS
// shifting
// missing tags in implicit context
// wrong type priority in for example ascription

object pneuma extends ScoverageModule {

    def name = "Pneuma Language"
    def version = "0.0.0"
    def scalaVersion = "3.2.2"
    def scoverageVersion = "2.0.7"
    def mainClass = T(Some("lambdacalculus.test"))
    def scalacOptions = Seq("-feature", "-deprecation")
    def ivyDeps = Agg(ivy"org.ow2.asm:asm:9.4")

    object test extends ScoverageTests with TestModule.ScalaTest {
        def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.14")
    }

}

object playground extends ScalaModule {

    def version = "0.0.0"
    def scalaVersion = "3.2.2"
    def mainClass = T(Some("lambdacalculus.test"))
    def scalacOptions = Seq("-feature", "-deprecation")
    def ivyDeps = Agg(ivy"org.ow2.asm:asm:9.4")
    def moduleDeps = Seq(pneuma)

}