import mill._
import scalalib._

/* version 0.0.0 -- core i */
// TODO module checking with expected shape DONE
// TODO module insertion preventation -- better module checking algorithm DONE
// TODO equivalence function DONE

/* version 0.0.0 -- core ii */
// TODO document pneuma.proto DONE
// TODO recheck complete structure
// TODO rethink the implicit naming conventions
// TODO make modules recursive
// TODO testing
// TODO introduce base types

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

object pneuma extends ScalaModule {

    def name = "Pneuma Language"
    def version = "0.0.0"
    def scalaVersion = "3.2.2"
    def mainClass = T(Some("lambdacalculus.test"))
    def scalacOptions = Seq("-feature", "-deprecation")
    def ivyDeps = Agg(ivy"org.ow2.asm:asm:9.4")

    object test extends Tests with TestModule.ScalaTest {
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