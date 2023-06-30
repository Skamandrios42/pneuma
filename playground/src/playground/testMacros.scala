package playground

enum Term derives HasMeta:
    case Var(meta: Metadata, x: String)
    case Abs(meta: Metadata, x: String, t: Term)
    case App(meta: Metadata, a: Term, b: Term)


@main def testMacros =

    val a = Term.Var(Metadata("text.txt", 42), "x")
    val b = Term.Abs(Metadata("text.txt", 45), "x", a)
    val c = Term.App(Metadata("text.txt", 44), b, a)

    println(a.meta)
    println(b.meta)
    println(c.meta)

    val meta = Metadata("other.txt", 12)

    println(a metaOf c)
    println(b metaOf c)
    println(c attach meta)
