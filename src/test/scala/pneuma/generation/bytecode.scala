package pneuma.generation

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Scanner
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.Handle
import java.lang.invoke.MethodType
import java.lang.invoke.CallSite
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodHandle
import org.objectweb.asm.Type
import java.lang.invoke.LambdaMetafactory

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
    println("done!")

def init(mv: MethodVisitor, name: String, descriptor: String)(argsOnStack: => Unit) = {
    mv.visitTypeInsn(NEW, name)
    mv.visitInsn(DUP)
    argsOnStack
    mv.visitMethodInsn(INVOKESPECIAL, name, "<init>", descriptor, false)
}

@main def generateLoop: Unit =

    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_8, ACC_PUBLIC, "Loop", null, "java/lang/Object", null)

    val sum = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "sum", "(II)I", null, null)

    sum.visitCode()
    sum.visitVarInsn(ILOAD, 0)
    sum.visitVarInsn(ILOAD, 1)
    sum.visitInsn(IADD)
    sum.visitInsn(IRETURN)
    sum.visitMaxs(0, 0)
    sum.visitEnd()

    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)

    // define method
    mv.visitCode()
    val begin = new Label
    val end = new Label
    // INIT loop index
    mv.visitInsn(ICONST_0)
    mv.visitVarInsn(ISTORE, 0)
    // CHECK loop condition
    mv.visitLabel(begin) // BEGIN
    mv.visitVarInsn(ILOAD, 0)
    mv.visitIntInsn(BIPUSH, 10)
    mv.visitJumpInsn(IF_ICMPGE, end)
    // BODY of loop
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    // init StringBuilder
    init(mv, "java/lang/StringBuilder", "()V") {}
    // append "n = "
    mv.visitLdcInsn("n = ")
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false)
    // append loop index
    mv.visitVarInsn(ILOAD, 0)
    mv.visitIntInsn(BIPUSH, 3)
    mv.visitMethodInsn(INVOKESTATIC, "Loop", "sum", "(II)I", false)
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;", false)
    // println(StringBuilder.toString)
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false)
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
    // increment loop variable and goto begin
    mv.visitIincInsn(0, 1)
    mv.visitJumpInsn(GOTO, begin)
    // RETURN of method
    mv.visitLabel(end) // END
    mv.visitInsn(RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    // defined method

    cw.visitEnd()
    Files.write(Paths.get("Loop.class"), cw.toByteArray)

@main def generateEchoLowLevel: Unit =

    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC, "Echo", null, "java/lang/Object", null)
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    mv.visitCode()
    // put `System.out` on the stack
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    // put `new Scanner` on the stack
    init(mv, "java/util/Scanner", "(Ljava/io/InputStream;)V") {
        // put `System.in` on the stack
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;")
    }
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

    // val scanner = new Scanner(System.in);
    // System.out.println(scanner.nextLine());

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

@main def generateDynamic(): Unit =

    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC, "Dynamic", null, "java/lang/Object", null)

    // main method
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    mv.visitCode()
    val mt = MethodType.methodType(classOf[CallSite], classOf[MethodHandles.Lookup], classOf[String], classOf[MethodType])
    val bootstrap = new Handle(H_INVOKESTATIC, "Target", "bootstrap", mt.toMethodDescriptorString, false)
    mv.visitInvokeDynamicInsn("alla", "()V", bootstrap)
    mv.visitInsn(RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    cw.visitEnd()

    Files.write(Paths.get("Dynamic.class"), cw.toByteArray)

@main def generateNoCast(): Unit =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC, "NoCast", null, "java/lang/Object", null)

    val print = writer.visitMethod(ACC_PUBLIC + ACC_STATIC, "print", "(Ljava/lang/Object;)V", null, null)
    print.visitCode()
    print.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    print.visitVarInsn(ALOAD, 0)
    print.visitIntInsn(BIPUSH, 2)
    print.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "get", "(I)Ljava/lang/Object;", true)
    print.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false)
    print.visitInsn(RETURN)
    print.visitMaxs(0, 0)
    print.visitEnd()

    val mv = writer.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    mv.visitCode()
    mv.visitLdcInsn("0 Hello world!")
    mv.visitLdcInsn("1 Hello world!")
    mv.visitLdcInsn("2 Hello world!")
    mv.visitMethodInsn(INVOKESTATIC, "java/util/List", "of", "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/util/List;", true)
    mv.visitMethodInsn(INVOKESTATIC, "NoCast", "print", "(Ljava/lang/Object;)V", false)
    mv.visitInsn(RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()

    writer.visitEnd()

    Files.write(Paths.get("NoCast.class"), writer.toByteArray)
    println("Done!")


@main def generateLambda(): Unit =

    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC, "Lambda", null, "java/lang/Object", null)

    // lambda method
    val add = cw.visitMethod(ACC_PRIVATE + ACC_STATIC, "add", "(I)I", null, null)

    add.visitCode()
    add.visitVarInsn(ILOAD, 0)
    add.visitIntInsn(BIPUSH, 10)
    add.visitInsn(IADD)
    add.visitInsn(IRETURN)
    add.visitMaxs(0, 0)
    add.visitEnd()

    // main method
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)

    mv.visitCode()
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

    // CREATION OF LAMBDA

    // type of the bootstrap method
    val mt = MethodType.methodType(
        classOf[CallSite],
        classOf[MethodHandles.Lookup],
        classOf[String],
        classOf[MethodType],
        classOf[MethodType],
        classOf[MethodHandle],
        classOf[MethodType],
    )

    val test = java.lang.invoke.LambdaMetafactory.metafactory

    // define metafactory as the bootstrap-method
    val bootstrap = new Handle(
        H_INVOKESTATIC,
        "java/lang/invoke/LambdaMetafactory", "metafactory",
        mt.toMethodDescriptorString, false
    )

    // create lambda object on stack
    mv.visitInvokeDynamicInsn(
        "apply", "()Ljava/util/function/Function;", bootstrap,
        Type.getType("(Ljava/lang/Object;)Ljava/lang/Object;"),
        new Handle(H_INVOKESTATIC, "Lambda", "add", "(I)I", false),
        Type.getType("(Ljava/lang/Integer;)Ljava/lang/Integer;")
    )
    // store object into locals
    mv.visitVarInsn(ASTORE, 1)


    mv.visitVarInsn(ALOAD, 1)
    // mv.visitIntInsn(BIPUSH, 32)
    // mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
    // mv.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false)
    mv.visitInsn(RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()

    cw.visitEnd()

    Files.write(Paths.get("Lambda.class"), cw.toByteArray)


/* public class InvokeDynamicCreator {

    public static void main(final String[] args) throws Exception {
        final String outputClassName = "kathik/Dynamic";
        try (FileOutputStream fos
                = new FileOutputStream(new File("target/classes/" + outputClassName + ".class"))) {
            fos.write(dump(outputClassName, "bootstrap", "()V"));
        }
    }

    public static byte[] dump(String outputClassName, String bsmName, String targetMethodDescriptor)
            throws Exception {
        final ClassWriter cw = new ClassWriter(0);
        MethodVisitor mv;

        // Setup the basic metadata for the bootstrap class
        cw.visit(V1_7, ACC_PUBLIC + ACC_SUPER, outputClassName, null, "java/lang/Object", null);

        // Create a standard void constructor
        mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();

        // Create a standard main method
        mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
        mv.visitCode();
        MethodType mt = MethodType.methodType(CallSite.class, MethodHandles.Lookup.class, String.class,
                MethodType.class);
        Handle bootstrap = new Handle(Opcodes.H_INVOKESTATIC, "kathik/InvokeDynamicCreator", bsmName,
                mt.toMethodDescriptorString());
        mv.visitInvokeDynamicInsn("runDynamic", targetMethodDescriptor, bootstrap);
        mv.visitInsn(RETURN);
        mv.visitMaxs(0, 1);
        mv.visitEnd();

        cw.visitEnd();

        return cw.toByteArray();
    }

    private static void targetMethod() {
        System.out.println("Hello World!");
    }

    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType type) throws NoSuchMethodException, IllegalAccessException {
        final MethodHandles.Lookup lookup = MethodHandles.lookup();
        // Need to use lookupClass() as this method is static
        final Class<?> currentClass = lookup.lookupClass();
        final MethodType targetSignature = MethodType.methodType(void.class);
        final MethodHandle targetMH = lookup.findStatic(currentClass, "targetMethod", targetSignature);
        return new ConstantCallSite(targetMH.asType(type));
    }
} */