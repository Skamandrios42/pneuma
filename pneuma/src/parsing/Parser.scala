package parsing

import general.Result
import general.{Metadata, HasMeta}
import parsing.Parser.GetPos

object Parser {

    def from[S, A](f: S => (A, S)): Parser[S, Nothing, A] = src0 =>
        val (result, src1) = f(src0)
        (Result.Success(result), src1)

    def apply[S] = new Creator[S]

    class Creator[S] {
        def success[A](value: => A): Parser[S, Nothing, A] = (Result.succeed(value), _)
        def failure[E](value: => E): Parser[S, E, Nothing] = (Result.fail(value), _)
        def result[E, A](value: => Result[E, A]): Parser[S, E, A] = (value, _)
    }

    trait GetPos[S] {
        // ADD FILE SUPPORT
        extension (self: S) def getIndex: Int
    }

}

trait Parser[S, +E, +A] extends (S => (Result[E, A], S)) {

    def filter[F](f: A => Boolean)(ex: S => F): Parser[S, E | F, A] = this(_) match
        case (Result.Success(value), src) if f(value) => (Result.succeed(value), src)
        case (Result.Success(value), src) => (Result.fail(ex(src)), src)
        case (Result.Failure(value), src) => (Result.Failure(value), src)

    def map[B](f: A => B): Parser[S, E, B] =
        this(_) match
            case (Result.Success(value), src) => (Result.Success(f(value)), src)
            case (Result.Failure(value), src) => (Result.Failure(value), src)
    def track[A1 >: A](using HasMeta[A1], GetPos[S]): Parser[S, E, A1] = src =>
        val start = src.getIndex
        val (res, newSrc) = this(src)
        val end = newSrc.getIndex
        (res.map(_.attach(Metadata(None, start, end))), newSrc)

    def tracked(file: Option[String])(using GetPos[S]): Parser[S, E, (A, Metadata)] = src =>
        val start = src.getIndex
        val (res, newSrc) = this(src)
        val end = newSrc.getIndex
        (res.map((_, Metadata(file, start, end))), newSrc)

    def transform[E1 >: E, A1 >: A, F, B](fa: A1 => B, fe: E1 => F): Parser[S, F, B] =
        this(_) match
            case (Result.Success(value), src) => (Result.Success(fa(value)), src)
            case (Result.Failure(value), src) => (Result.Failure(value.map(fe)), src)

    def flatMap[B, F](f: A => Parser[S, F, B]): Parser[S, E | F, B] =
        this(_) match
            case (Result.Success(value), src) => f(value)(src)
            case (Result.Failure(value), src) => (Result.Failure(value), src)

    def <*>[F, B](that: => Parser[S, F, B]): Parser[S, E | F, (A, B)] =
        for a <- this
            b <- that
        yield (a, b)

    def *>[F, B](that: => Parser[S, F, B]): Parser[S, E | F, B] =
        for _ <- this
            b <- that
        yield b

    def <*[F, B](that: => Parser[S, F, B]): Parser[S, E | F, A] =
        for a <- this
            _ <- that
        yield a

    def opt: Parser[S, Nothing, Option[A]] = this.map(Some(_)).orElse(Parser[S].success(None))

    def orElse[F, B](that: => Parser[S, F, B]): Parser[S, F, A | B] = src =>
        this(src) match
            case (Result.Success(value), src) => (Result.Success(value), src)
            case (Result.Failure(value), _)   => that(src)

    def or[E1 >: E, F, B](that: => Parser[S, F, B])(using order: Ordering[E1 | F]): Parser[S, E | F, A | B] = src =>
        this(src) match
            case (Result.Success(value), src1) => (Result.Success(value), src1)
            case (Result.Failure(e), src1)   => that(src) match
                case (Result.Success(value), src2) => (Result.Success(value), src2)
                case (Result.Failure(f), src2)   => if order.gt(e.min, f.min) then (Result.Failure(e), src1) else (Result.Failure(f), src1)

    def repeat: Parser[S, Nothing, List[A]] = (
        for head <- this
            tail <- repeat
        yield head :: tail) orElse Parser[S].success(List.empty)

    def repeatsep[F, B, A1 >: A](that: Parser[S, F, B]): Parser[S, E, List[A]] = 
        for head <- this
            tail <- that alternate this
        yield head :: tail.map(_(1))

    def alternate[F, B](that: Parser[S, F, B]): Parser[S, F, List[(A, B)]] =
        this(_) match
            case (Result.Success(a), src) => (
                for b    <- that
                    rest <- this alternate that
                yield (a, b) :: rest)(src)
            case (Result.Failure(e), src) => (Result.Success(List.empty), src)

    def fold[A1 >: A](op: (A1, A1) => A1): Parser[S, E, A1] =
        for head <- this
            tail <- this.repeat
        yield tail.foldLeft(head)(op)

    def foldsep[F, B, A1 >: A](that: Parser[S, F, B])(op: (A1, B, A1) => A1): Parser[S, E, A1] =
        for head <- this
            tail <- that alternate this
        yield tail.foldLeft[A1](head) { case (a0, (b, a1)) => op(a0, b, a1) }

    override def toString = "<parser>"
}
