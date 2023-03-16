import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill._
import scalalib._
import mill.contrib.scoverage.ScoverageModule

/* version 0.0.0 -- core i */
// TODO module checking with expected shape DONE
// TODO module insertion preventation -- better module checking algorithm DONE
// TODO equivalence function DONE

/* version 0.0.0 -- core ii */
// TODO document pneuma.proto DONE
// TODO make modules recursive DONE
// TODO rethink the implicit naming conventions DONE
// TODO introduce natural numbers DONE
// TODO testing
// TODO recheck complete structure

/* version 0.0.0 -- infrastructure */
// TODO build a parser
// TODO build a bytecode generator

/* version 0.0.0 -- release */
// TODO initialize repository
// TODO make publishing setup
// TODO clean project

/* version 0.1.0 */
// TODO inductive datatypes
// TODO typeclasses for literal macros
// TODO mutual recursive modules

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