package pneuma.proto

import org.objectweb.asm.Opcodes.*
import generation.defineClass
import generation.defineMethod
import generation.mkLambda
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.Label
import generation.Counter

object Generator {
    def bytecode(name: String, term: Term) =
        defineClass(V17, ACC_PUBLIC, name) { cw =>
            cw.defineMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V") { mv =>
                generate(name, cw, mv, term, Map.empty, Counter(), Counter())
                mv.visitInsn(RETURN)
            }
        }

    extension (self: Map[Int, Int]) def >>(amount: Int) = self.map {
        case (a, b) => (a + 1, b)
    }

    def generate(name: String, cw: ClassWriter, mv: MethodVisitor, term: Term, context: Map[Int, Int], anonCounter: Counter, modCounter: Counter): Unit = term match
        case Term.Var(x, tagged, _) =>
            // puts local `x` on the stack
            mv.visitVarInsn(ALOAD, context(x))
        case Term.Abs(t, _, _) =>
            val anon = f"anon$$${anonCounter.next()}%04d"
            val sig =  s"(${"Ljava/lang/Object;" * (context.size + 1)})Ljava/lang/Object;"
            // generate `body` as static function
            cw.defineMethod(ACC_PRIVATE + ACC_STATIC, anon, sig) { mv =>
                generate(name, cw, mv, t, (context >> 1) + (0 -> context.size), anonCounter, modCounter)
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

        case Term.App(t1, t2, _) =>
            generate(name, cw, mv, t1, context, anonCounter, modCounter) // put `abs` on the stack
            generate(name, cw, mv, t2, context, anonCounter, modCounter) // put `arg` on the stack
            mv.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)
        
        case Term.Module(fields, _) =>
            // println(fields)
            mv.visitTypeInsn(NEW, "java/util/HashMap")
            mv.visitInsn(DUP)
            mv.visitMethodInsn(INVOKESPECIAL, "java/util/HashMap", "<init>", "()V", false)
            val index = modCounter.next()
            for Term.ModElem(ident, term, _) <- fields do
                // println(s"for $ident : $context")
                mv.visitInsn(DUP)
                mv.visitLdcInsn(ident)
                val anon = f"mod$$$ident$$${index}%04d"
                val sig = s"(${"Ljava/lang/Object;" * (context.size + 1)})Ljava/lang/Object;"
                // generate `body` as static function
                cw.defineMethod(ACC_PRIVATE + ACC_STATIC, anon, sig) { mv =>
                    generate(name, cw, mv, term, (context >> 1) + (0 -> context.size), anonCounter, modCounter)
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

        case Term.Get(t, field, _) =>
            // erzeuge das Modul
            generate(name, cw, mv, t, context, anonCounter, modCounter)
            mv.visitTypeInsn(CHECKCAST, "java/util/HashMap");
            // dupliziere es
            mv.visitInsn(DUP)
            mv.visitVarInsn(ASTORE, context.size)
            mv.visitLdcInsn(field)
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/HashMap", "get", "(Ljava/lang/Object;)Ljava/lang/Object;", false)
            mv.visitVarInsn(ALOAD, context.size)
            mv.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true)

        case Term.As(te, ty, _) =>
            generate(name, cw, mv, te, context, anonCounter, modCounter)

        case Term.Nat(value, _) =>
            mv.visitLdcInsn(value)
            // mv.visitIntInsn(BIPUSH, value)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);

        case Term.Succ(t, _) =>
            generate(name, cw, mv, t, context, anonCounter, modCounter)
            mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
            mv.visitIntInsn(BIPUSH, 1)
            mv.visitInsn(IADD)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);

        case Term.Debug(t, _) =>
            // get out field and evaluate `t`
            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            generate(name, cw, mv, t, context, anonCounter, modCounter)
            mv.visitVarInsn(ASTORE, context.size)
            mv.visitVarInsn(ALOAD, context.size)
            // print `t` and put it on stack afterwards
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false)
            mv.visitVarInsn(ALOAD, context.size)

        case Term.Match(t, onZero, onSucc, _, _) =>
            val second = new Label
            val end = new Label
            generate(name, cw, mv, t, context, anonCounter, modCounter)
            mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
            mv.visitInsn(DUP)
            mv.visitIntInsn(BIPUSH, 0)
            mv.visitJumpInsn(IF_ICMPNE, second)

            // on zero
            mv.visitInsn(POP)
            generate(name, cw, mv, onZero, context, anonCounter, modCounter)
            mv.visitJumpInsn(GOTO, end)
            // on succ
            mv.visitLabel(second)
            mv.visitIntInsn(BIPUSH, 1)
            mv.visitInsn(ISUB)
            mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
            mv.visitVarInsn(ASTORE, context.size)
            generate(name, cw, mv, onSucc, (context >> 1) + (0 -> context.size), anonCounter, modCounter)

            mv.visitLabel(end)
        case other =>
            // println(s"WARNING: $other got erased!")
            mv.visitInsn(ACONST_NULL)
            //mv.visitFieldInsn(GETSTATIC, "Type", "instance", "LType;")
}