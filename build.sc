import mill._
import scalalib._

object pneuma extends ScalaModule {

    def name = "Pneuma Language"
    def version = "0.0.0"
    def scalaVersion = "3.2.1"
    def mainClass = T(Some("lambdacalculus.test"))
    def scalacOptions = Seq("-feature", "-deprecation")
    def ivyDeps = Agg(ivy"org.ow2.asm:asm:9.4")

    object test extends Tests with TestModule.ScalaTest {
        def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.14")
    }

}