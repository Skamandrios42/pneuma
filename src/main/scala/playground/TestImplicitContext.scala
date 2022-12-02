package playground

import scala.collection.mutable.ListBuffer

object TestImplicitContext extends App {


    case class Config(name: String, age: Int, subjects: List[String])

    class ConfigSpace {

        var name: String = null
        var age: Int = -1
        var subjects = ListBuffer.empty[String]

        def get: Config = Config(name, age, subjects.toList)
    }

    object name {
        def :=(value: String)(using space: ConfigSpace) = space.name = value
    }
    object age {
        def :=(value: Int)(using space: ConfigSpace) = space.age = value
    }
    object subjects {
        def +=(value: String)(using space: ConfigSpace) = space.subjects += value
        def ++=(values: Seq[String])(using space: ConfigSpace) = space.subjects ++= values
    }

    def configure(block: ConfigSpace ?=> Unit): Config =
        given space: ConfigSpace with {}
        block
        space.get




    val config = configure {
        name := "Theo"
        age := 18
        subjects ++= Seq("Physik", "Informatik", "Mathematik")
        subjects += "IMP"
    }


    println(config)

}
