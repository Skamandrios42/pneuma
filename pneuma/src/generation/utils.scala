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

def defineClass(version: Int, access: Int, name: String, signature: String = null,
                superclass: String = "java/lang/Object", interfaces: Array[String] = null)
               (definition: ClassWriter => Unit) =
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS + ClassWriter.COMPUTE_FRAMES)
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

class Namer(val base: String) {
    private var index = 0
    def next() =
        val name = f"$base$index%04d"
        index += 1
        name
}
