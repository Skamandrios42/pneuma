package generation

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.Opcodes
import java.nio.file.Files
import java.nio.file.Paths
import java.lang.invoke.CallSite
import java.lang.invoke.MethodHandle
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType
import org.objectweb.asm.Type
import scala.collection.mutable.ListBuffer


def defineClass(version: Int, access: Int, name: String, signature: String = null,
                superclass: String = "java/lang/Object", interfaces: Array[String] = null)
               (definition: ClassWriter => Unit) =
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(version, access, name, signature, superclass, interfaces)
    definition(cw)
    cw.visitEnd()
    Files.write(Paths.get(s"$name.class"), cw.toByteArray)

extension (cw: ClassWriter)
    def defineMethod(access: Int, name: String, descriptor: String,
                     signature: String = null, exceptions: Array[String] = null)
                    (definition: MethodVisitor => Unit) =
        val mv = cw.visitMethod(access, name, descriptor, signature, exceptions)
        mv.visitCode()
        definition(mv)
        mv.visitMaxs(0, 0)
        mv.visitEnd()

// mv.visitInvokeDynamicInsn(
//                 "apply", s"(${"Ljava/lang/Object;" * context.size})Ljava/util/function/Function;", bootstrap,
//                 Type.getType(funDescriptor),
//                 Handle(H_INVOKESTATIC, name, anon, sig, false),
//                 Type.getType(funDescriptor)
//             )

extension (mv: MethodVisitor)
    def mkLambda(nameOfSAM: String, descriptorOfDynamicCall: String, implementation: Handle) =

        val funType = Type.getType(MethodType.methodType(classOf[Object], classOf[Object]).toMethodDescriptorString())

        val bootType = MethodType.methodType(
            classOf[CallSite],
            classOf[MethodHandles.Lookup],
            classOf[String],
            classOf[MethodType],
            classOf[MethodType],
            classOf[MethodHandle],
            classOf[MethodType],
        )
        val bootstrap = new Handle(
            Opcodes.H_INVOKESTATIC,
            "java/lang/invoke/LambdaMetafactory", "metafactory",
            bootType.toMethodDescriptorString, false
        )
        mv.visitInvokeDynamicInsn(nameOfSAM, descriptorOfDynamicCall, bootstrap, funType, implementation, funType)

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