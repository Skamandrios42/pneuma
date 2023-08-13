package pneuma.proto

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process.*

class SourceTests extends AnyFunSuite {

    test("math.pneuma") {
        Compiler.main(Array("tests/math.pneuma"))
        Seq("java", "tests/math").!
    }

}