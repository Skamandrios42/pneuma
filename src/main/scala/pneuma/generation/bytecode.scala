package pneuma.generation

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Scanner
import org.objectweb.asm.MethodVisitor

def defineClass(name: String, sup: String, saving: Boolean)(code: ClassWriter => Unit) = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC, "Echo", null, sup, null)
    code(writer)
    writer.visitEnd()
    if saving then Files.write(Paths.get(s"$name.class"), writer.toByteArray)
    writer
}

def defineMethod(visitor: MethodVisitor)(code: MethodVisitor => Unit) =
    visitor.visitCode()
    code(visitor)
    visitor.visitMaxs(0, 0)
    visitor.visitEnd()

@main def generateEcho: Unit =

    defineClass("Echo", "java/lang/Object", saving = true) { cw =>
        
        defineMethod(cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)) { mv =>
            // put `System.out` on the stack
            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            // put `new Scanner` on the stack
            mv.visitTypeInsn(NEW, "java/util/Scanner")
            // duplicate top entry on the stack
            mv.visitInsn(DUP)
            // put `System.in` on the stack
            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;")
            // call `init` of Scanner, consumes `System.in` and one `Scanner`
            mv.visitMethodInsn(INVOKESPECIAL, "java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V", false)
            // call nextLine on the remaining scanner object
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Scanner", "nextLine", "()Ljava/lang/String;", false)
            // make the string uppercase
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "toUpperCase", "()Ljava/lang/String;", false)
            // call println on the resulting string and System.out
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
            // return
            mv.visitInsn(RETURN)
        }

    }

@main def generateEchoLowLevel: Unit =

    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC, "Echo", null, "java/lang/Object", null)
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    mv.visitCode()
    // put `System.out` on the stack
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    // put `new Scanner` on the stack
    mv.visitTypeInsn(NEW, "java/util/Scanner")
    // duplicate top entry on the stack
    mv.visitInsn(DUP)
    // put `System.in` on the stack
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;")
    // call `init` of Scanner, consumes `System.in` and one `Scanner`
    mv.visitMethodInsn(INVOKESPECIAL, "java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V", false)
    // call nextLine on the remaining scanner object
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Scanner", "nextLine", "()Ljava/lang/String;", false)
    // make the string uppercase
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "toUpperCase", "()Ljava/lang/String;", false)
    // call println on the resulting string and System.out
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
    // return
    mv.visitInsn(RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    cw.visitEnd()
    Files.write(Paths.get("Echo.class"), cw.toByteArray)


@main def generate(): Unit = 

    val scanner = new Scanner(System.in);
    System.out.println(scanner.nextLine());

    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC, "GeneratedClass", null, "java/lang/Object", null)
    val visitor = writer.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    
    visitor.visitCode()
    visitor.visitLdcInsn("Hello world!")
    visitor.visitVarInsn(ASTORE, 0)
    visitor.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    visitor.visitVarInsn(ALOAD, 0)
    visitor.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
    visitor.visitInsn(RETURN)
    visitor.visitMaxs(0, 0)
    visitor.visitEnd()
    writer.visitEnd()

    Files.write(Paths.get("GeneratedClass.class"), writer.toByteArray)
