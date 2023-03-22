import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill._
import scalalib._
import mill.contrib.scoverage.ScoverageModule

object pneuma extends ScoverageModule {

    def name = "Pneuma Language"
    def version = "0.0.0"
    def scalaVersion = "3.2.2"
    def scoverageVersion = "2.0.7"
    def mainClass = T(Some("pneuma.proto.Compiler"))
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