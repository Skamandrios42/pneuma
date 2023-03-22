package pneuma.proto

import org.objectweb.asm.Opcodes.*
import generation.defineClass
import generation.defineMethod
import generation.mkLambda
import generation.Namer
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.Label

object Generator {
    def apply(name: String, term: Term) =
        defineClass(V17, ACC_PUBLIC, name) { cw =>
            cw.defineMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V") { mv =>
                generate(name, cw, mv, term, Map.empty, Namer("anon$"))
                mv.visitInsn(RETURN)
            }
        }

    extension (self: Map[Int, Int]) def >>(amount: Int) = self.map {
        case (a, b) => (a + 1, b)
    }

    def generate(name: String, cw: ClassWriter, mv: MethodVisitor, term: Term, context: Map[Int, Int], namer: Namer): Unit = term match
        case Term.Var(x, tagged) =>
            // puts local `x` on the stack
            println(context)
            mv.visitVarInsn(ALOAD, context(x))
        case Term.Abs(t) =>
            val anon = namer.next()
            val sig =  s"(${"Ljava/lang/Object;" * (context.size + 1)})Ljava/lang/Object;"
            // generate `body` as static function
            cw.defineMethod(ACC_PRIVATE + ACC_STATIC, anon, sig) { mv =>
                generate(name, cw, mv, t, (context >> 1) + (0 -> context.size), namer)
                mv.visitInsn(ARETURN)
            }
            // load all local variables
            for (idx -> loc) <- context do
                mv.visitVarInsn(ALOAD, loc)
            // create lambda object on stack using invokedynamic (captures all local variables)
            mv.mkLambda("apply",
                s"(${"Ljava/lang/Object;" * context.size})Ljava/util/function/Function;",
                Handle(H_INVOKESTATIC, name, anon, sig, false)
            )

        case Term.App(t1, t2) =>
            generate(name, cw, mv, t1, context, namer) // put `abs` on the stack
            generate(name, cw, mv, t2, context, namer) // put `arg` on the stack
            mv.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)
        case Term.Typ =>
            println("'TYP' IS NOT GENERATED")
        case Term.Phi =>
            println("'PHI' IS NOT GENERATED")
        case Term.Pro(t1, t2) =>
            println("'PRO' IS NOT GENERATED")
        case Term.Imp(t1, t2) =>
            println("'IMP' IS NOT GENERATED")
        
        case Term.Module(fields) =>
            println(fields)
            mv.visitTypeInsn(NEW, "java/util/HashMap")
            mv.visitInsn(DUP)
            mv.visitMethodInsn(INVOKESPECIAL, "java/util/HashMap", "<init>", "()V", false)
            for Term.ModElem(ident, term, _) <- fields do
                println(s"for $ident : $context")
                mv.visitInsn(DUP)
                mv.visitLdcInsn(ident)

                val anon = namer.next()
                val sig = s"(${"Ljava/lang/Object;" * (context.size + 1)})Ljava/lang/Object;"
                // generate `body` as static function
                cw.defineMethod(ACC_PRIVATE + ACC_STATIC, anon, sig) { mv =>
                    generate(name, cw, mv, term, (context >> 1) + (0 -> context.size), namer)
                    mv.visitInsn(ARETURN)
                }
                // load all local variables
                for (idx -> loc) <- context do
                    mv.visitVarInsn(ALOAD, loc)
                // create lambda object on stack using invokedynamic (captures all local variables)
                mv.mkLambda("apply",
                    s"(${"Ljava/lang/Object;" * context.size})Ljava/util/function/Function;",
                    Handle(H_INVOKESTATIC, name, anon, sig, false)
                )

                //generate(name, cw, mv, term, (context >> 1) + (0 -> context.size), namer)
                mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/HashMap", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", false)
                mv.visitInsn(POP)
            //     mv.visitMethodInsn(INVOKESTATIC, "Test", "debug", "(Ljava/lang/Object;)Ljava/lang/Object;", false)
            //     mv.visitTypeInsn(CHECKCAST, "java/util/HashMap");
            mv.visitMethodInsn(INVOKESTATIC, "Test", "debug", "(Ljava/lang/Object;)Ljava/lang/Object;", false)
            mv.visitTypeInsn(CHECKCAST, "java/util/HashMap");

        case Term.Interface(fields) =>
            println("'INTERFACE' IS NOT GENERATED")

        case Term.Get(t, field) =>
            // erzeuge das Modul
            generate(name, cw, mv, t, context, namer)
            mv.visitTypeInsn(CHECKCAST, "java/util/HashMap");
            // dupliziere es
            mv.visitInsn(DUP)
            mv.visitVarInsn(ASTORE, context.size)
            mv.visitLdcInsn(field)
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/HashMap", "get", "(Ljava/lang/Object;)Ljava/lang/Object;", false)
            mv.visitVarInsn(ALOAD, context.size)
            mv.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)

        case Term.As(te, ty) =>
            generate(name, cw, mv, te, context, namer)

        case Term.NatType =>
            println("'NATTYPE' IS NOT GENERATED")

        case Term.Nat(value) =>
            mv.visitIntInsn(BIPUSH, value)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);

        case Term.Succ(t) =>
            generate(name, cw, mv, t, context, namer)
            mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
            mv.visitIntInsn(BIPUSH, 1)
            mv.visitInsn(IADD)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
        
        case Term.Debug(t) =>
            // get out field and evaluate `t`
            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            generate(name, cw, mv, t, context, namer)
            mv.visitVarInsn(ASTORE, context.size)
            mv.visitVarInsn(ALOAD, context.size)
            // print `t` and put it on stack afterwards
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false)
            mv.visitVarInsn(ALOAD, context.size)
        
        case Term.Match(t, onZero, onSucc) =>
            val second = new Label
            val end = new Label
            generate(name, cw, mv, t, context, namer)
            mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
            mv.visitInsn(DUP)
            mv.visitIntInsn(BIPUSH, 0)
            mv.visitJumpInsn(IF_ICMPNE, second)

            // on zero
            mv.visitInsn(POP)
            generate(name, cw, mv, onZero, context, namer)
            mv.visitJumpInsn(GOTO, end)
            // on succ
            mv.visitLabel(second)
            mv.visitIntInsn(BIPUSH, 1)
            mv.visitInsn(ISUB)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
            mv.visitVarInsn(ASTORE, context.size)
            generate(name, cw, mv, onSucc, (context >> 1) + (0 -> context.size), namer)

            mv.visitLabel(end)
}