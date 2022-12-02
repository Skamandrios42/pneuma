package implicits

enum Term {
  case Num(x: Int)
  case EVar(x: String)
  case IVar(y: String)
  case Abs(x: String, body: Term)
  case App(abs: Term, arg: Term)
  case Query
  case ELet(x: String, in: Type, arg: Term, body: Term)
  case ILet(in: Type, arg: Term, body: Term)
}

enum Type(val full: Boolean) {
  case Num extends Type(false)
  //case Var(x: String) extends Type(false)
  case EFun(in: Type, out: Type) extends Type(false)
  case IFun(in: Type, out: Type) extends Type(true)
  //case PFun(x: String, out: Type) extends Type(true)
}

case object NoRuleException extends Exception("no rule applies")

trait Typing {

  type EContext = List[(Term.EVar | Term.IVar, Type)]

  def synth(context: EContext, te: Term): (Type, Term)
  def check(context: EContext, te: Term, ty: Type): Term
  def elim(context: EContext, te: Term, ty: Type): (Type, Term)
  def query(context: EContext, ty: Type, _context: EContext): Term

}

object TypingImpl extends Typing {

  object Namer {
    private var index = 0
    def get() =
      val name = s"imp$$$index"
      index += 1
      name
  }

  def synth(context: EContext, te: Term): (Type, Term) = te match
    case Term.Num(n) => (Type.Num, Term.Num(n))
    case Term.IVar(y) => elim(context, Term.IVar(y), context.find {
      case (Term.IVar(y0), ty) => y == y0
      case _ => false
    }.get(1))
    case Term.EVar(x) => elim(context, Term.EVar(x), context.find {
      case (Term.EVar(x0), ty) => x == x0
      case _ => false
    }.get(1))
    case Term.App(t0, t1) =>
      val (Type.EFun(in, out), t0te) = synth(context, t0)
      val t1te = check(context, t1, in)
      (out, Term.App(t0te, t1te))
    case Term.ELet(x, in, arg, body) =>
      val _s = check(context, arg, in)
      val (body_ty, body_te) = synth((Term.EVar(x), in) :: context, body)
      (body_ty, Term.App(Term.Abs(x, body_te), _s))
    case Term.ILet(in, arg, body) =>
      val x = Namer.get()
      val _s = check(context, arg, in)
      val (body_ty, body_te) = synth((Term.IVar(x), in) :: context, body)
      (body_ty, Term.App(Term.Abs(x, body_te), _s))
    case Term.Query =>
      println("tried QUERY")
      throw NoRuleException
    case _ => throw NoRuleException


  def check(context: EContext, te: Term, ty: Type): Term = (te, ty) match
    case (Term.ELet(x, in, arg, body), ty) =>
      println("check in ELet")
      val _s = check(context, arg, in)
      //val (body_ty, body_te) = synth((Term.EVar(x), in) :: context, body)
      Term.App(Term.Abs(x, check((Term.EVar(x), in) :: context, body, ty)), _s)
      //if body_ty == ty then Term.App(Term.Abs(x, body_te), _s) else throw NoRuleException
    case (Term.ILet(in, arg, body), ty) =>
      val x = Namer.get()
      val _s = check(context, arg, in)
      println(s"check in ILet: $context -- $arg -- $in --> $_s")
      //val (body_ty, body_te) = synth((Term.IVar(x), in) :: context, body)
      Term.App(Term.Abs(x, check((Term.IVar(x), in) :: context, body, ty)), _s)
      //if body_ty == ty then Term.App(Term.Abs(x, body_te), _s) else throw NoRuleException
    case (Term.Abs(x, body), Type.EFun(in, out)) => Term.Abs(x, check((Term.EVar(x), in) :: context, body, out))
    case (te, Type.IFun(in, out)) =>
      val name = Namer.get()
      Term.Abs(name, check((Term.IVar(name), in) :: context, te, out))
    case (te, ty) if !ty.full =>
      lazy val (s_ty, s_te) = synth(context, te)
      if te == Term.Query then query(context, ty, context)
      else if s_ty == ty then s_te
      else throw NoRuleException
    case _ => throw NoRuleException


  def elim(context: EContext, te: Term, ty: Type): (Type, Term) = ty match
    case _ if !ty.full      => (ty, te)
    case Type.IFun(in, out) => elim(context, Term.App(te, query(context, in, context)), out)
    case _                  => throw NoRuleException

  def query(context: EContext, ty: Type, _context: EContext): Term = _context match
    case (Term.EVar(x), s) :: rest => query(context, ty, rest)
    case (Term.IVar(y), s) :: rest =>
      val (s_ty, s_te) = synth(context, Term.IVar(y))
      if s_ty == ty then s_te else query(context, ty, rest)
    case Nil => throw NoRuleException
}

@main def testTyping = {

  import Term.*
  import Type.{Num as NUM, *}

  val typing: Typing = TypingImpl

  val term = ELet("program", NUM, ILet(NUM, Num(42), Query), EVar("program"))

  println(typing.check(Nil, term, NUM))

}

// object TypingOld {

//     type EContext = Map[String, Type]
//     type IContext = Set[Type]
//     type PContext = Set[String]

//     def synthesize(econ: EContext, icon: IContext, pcon: PContext, te: Term): Type = te match
//         case Term.EVar(x) => econ(x)
//         case Term.IVar => ???
//         case Term.App(abs, arg) =>
//             val Type.EFun(in, out) = synthesize(econ, icon, pcon, abs)
//             check(econ, icon, pcon, arg, in)
//             out
//         case Term.ELet(x, in, arg, body) =>
//             check(econ, icon, pcon, arg, in)
//             synthesize(econ + (x -> in), icon, pcon, body).ensuring(!_.full)
//         case Term.ILet(in, arg, body) =>
//             check(econ, icon, pcon, arg, in)
//             synthesize(econ, icon + in, pcon, body).ensuring(!_.full)
//         case te => ???
//         // case Term.Abs(x, body) => ???
//         // case Term.Query => ???


//     def check(econ: EContext, icon: IContext, pcon: PContext, te: Term, ty: Type): Unit = (te, ty) match
//         case (Term.Abs(x, t), Type.EFun(in, out)) =>
//             check(econ + (x -> in), icon, pcon, t, out)
//         case (t, Type.IFun(in, out)) =>
//             check(econ, icon + in, pcon, t, out)
//         case (t, Type.PFun(x, out)) =>
//             check(econ, icon, pcon + x, t, out)
//         case (te, ty)
//             if !ty.full && synthesize(econ, icon, pcon, te) == ty =>
//         case _ => throw NoRuleException
// }