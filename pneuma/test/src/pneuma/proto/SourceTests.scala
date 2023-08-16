package pneuma.proto

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process.*

class SourceTests extends AnyFunSuite {

    test("math.pneuma") {
        Compiler.main(Array("tests/math.pneuma"))
        Seq("java", "tests/math").!
    }

    test("vector.pneuma") {
        Compiler.main(Array("tests/vector.pneuma"))
        Seq("java", "tests/vector").!
    }

    test("high.pneuma") {
        Compiler.main(Array("tests/high.pneuma"))
        Seq("java", "tests/high").!
    }

}