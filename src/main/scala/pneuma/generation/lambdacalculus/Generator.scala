package pneuma.generation.lambdacalculus

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*
import java.nio.file.Files
import java.nio.file.Paths
import org.objectweb.asm.MethodVisitor
import java.util.UUID
import java.lang.invoke.MethodType
import java.lang.invoke.CallSite
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodHandle
import org.objectweb.asm.Handle
import org.objectweb.asm.Type

object Generator {

    def apply(name: String, term: Term) =
        val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
        writer.visit(V1_8, ACC_PUBLIC, name, null, "java/lang/Object", null)
        val visitor = writer.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
        visitor.visitCode()
        generate(name, writer, visitor, term, Nil, Namer("anon_"))
        visitor.visitInsn(RETURN)
        visitor.visitMaxs(0, 0)
        visitor.visitEnd()
        writer.visitEnd()

        Files.write(Paths.get(s"$name.class"), writer.toByteArray)

    def generate(name: String, cw: ClassWriter, mv: MethodVisitor, term: Term, context: List[String], names: Namer): Unit = term match
        case Term.Var(x) =>
            // puts local `x` on the stack
            mv.visitVarInsn(ALOAD, context.indexOf(x))
        case Term.Abs(x, body) => 
            // generate `body` as static function
            val anon = names.next()
            val sig =  s"(${"Ljava/lang/Object;" * (context.size + 1)})Ljava/lang/Object;"
            val local_mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC, anon, sig, null, null)
            local_mv.visitCode()
            generate(name, cw, local_mv, body, context.appended(x), names)
            local_mv.visitInsn(ARETURN)
            local_mv.visitMaxs(0, 0)
            local_mv.visitEnd()
            // load all local variables
            context.indices.foreach { mv.visitVarInsn(ALOAD, _) }
            // create lambda object on stack using invokedynamic (captures all local variables)
            mv.visitInvokeDynamicInsn(
                "apply", s"(${"Ljava/lang/Object;" * context.size})Ljava/util/function/Function;", bootstrap, 
                Type.getType(funDescriptor),
                new Handle(H_INVOKESTATIC, name, anon, sig, false),
                Type.getType(funDescriptor)
            )
        case Term.App(abs, arg) =>
            generate(name, cw, mv, abs, context, names) // put `abs` on the stack
            generate(name, cw, mv, arg, context, names) // put `arg` on the stack
            mv.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", funDescriptor, true)
        case Term.Str(value) =>
            mv.visitLdcInsn(value)
        case Term.Print(t) =>
            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            generate(name, cw, mv, t, context, names)
            mv.visitVarInsn(ASTORE, context.length)
            mv.visitVarInsn(ALOAD, context.length)
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false)
            mv.visitVarInsn(ALOAD, context.length)

    val funDescriptor = MethodType.methodType(classOf[Object], classOf[Object]).toMethodDescriptorString()

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
        H_INVOKESTATIC, 
        "java/lang/invoke/LambdaMetafactory", "metafactory", 
        bootType.toMethodDescriptorString, false
    )
    
    class Namer(val base: String) {
        private var index = 0
        def next() = 
            val name = f"$base$index%04d"
            index += 1
            name
    }



    def test(name: String, term: Term) =
        val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
        writer.visit(V1_8, ACC_PUBLIC, name, null, "java/lang/Object", null)

        val debug = writer.visitMethod(ACC_PUBLIC + ACC_STATIC, "debug", "(Ljava/lang/Object;)Ljava/lang/Object;", null, null)

        debug.visitCode()
        debug.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
        debug.visitVarInsn(ALOAD, 0)
        debug.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false)
        debug.visitVarInsn(ALOAD, 0)
        debug.visitInsn(ARETURN)
        debug.visitMaxs(0, 0)
        debug.visitEnd()

        val visitor = writer.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
        visitor.visitCode()
        generate(name, writer, visitor, term, Nil, Namer("anon_"))
        visitor.visitInvokeDynamicInsn(
            "apply", "()Ljava/util/function/Function;", bootstrap, 
            Type.getType("(Ljava/lang/Object;)Ljava/lang/Object;"),
            new Handle(H_INVOKESTATIC, name, "debug", "(Ljava/lang/Object;)Ljava/lang/Object;", false),
            Type.getType("(Ljava/lang/Object;)Ljava/lang/Object;")
        )
        visitor.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)
        visitor.visitInvokeDynamicInsn(
            "apply", "()Ljava/util/function/Function;", bootstrap, 
            Type.getType("(Ljava/lang/Object;)Ljava/lang/Object;"), 
            new Handle(H_INVOKEVIRTUAL, "java/lang/String", "toUpperCase", "()Ljava/lang/String;", false),
            Type.getType("(Ljava/lang/String;)Ljava/lang/String;")
        )
        visitor.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)
        visitor.visitLdcInsn("Hey!")
        visitor.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)

        visitor.visitInsn(RETURN)
        visitor.visitMaxs(0, 0)
        visitor.visitEnd()
        writer.visitEnd()

        Files.write(Paths.get(s"$name.class"), writer.toByteArray)

}