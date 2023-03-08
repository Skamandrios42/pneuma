package parsing

import Parser.Result.{Success, Failure}
import Parser.Result

object Parser {

    enum Result[+E, +A] {
        case Success(value: A)
        case Failure(value: E)
        def map[B](f: A => B): Result[E, B] = this match
            case Success(value) => Success(f(value))
            case Failure(value) => Failure(value)
        def flatMap[B, F](f: A => Result[F, B]): Result[E | F, B]  = this match
            case Success(value) => f(value)
            case Failure(value) => Failure(value)
        def foreach[B](f: A => Unit): Unit = this match
            case Success(value) => f(value)
            case _ =>
        
    }

    def from[S, A](f: S => (A, S)): Parser[S, Nothing, A] = src0 =>
        val (result, src1) = f(src0)
        (Success(result), src1)

    def apply[S] = new Creator[S]

    class Creator[S] {
        def success[A](value: => A): Parser[S, Nothing, A] = (Success(value), _)
        def failure[E](value: => E): Parser[S, E, Nothing] = (Failure(value), _)
        def result[E, A](value: => Result[E, A]): Parser[S, E, A] = (value, _)
    }

}

trait Parser[S, +E, +A] extends (S => (Result[E, A], S)) {

    def map[B](f: A => B): Parser[S, E, B] =
        this(_) match
            case (Success(value), src) => (Success(f(value)), src)
            case (Failure(value), src) => (Failure(value), src)

    def recover[E1 >: E, B](f: E1 => B): Parser[S, Nothing, A | B] = 
        this(_) match
            case (Success(value), src) => (Success(value), src)
            case (Failure(value), src) => (Success(f(value)), src)

    def transform[E1 >: E, A1 >: A, F, B](fa: A1 => B, fe: E1 => F): Parser[S, F, B] =
        this(_) match
            case (Success(value), src) => (Success(fa(value)), src)
            case (Failure(value), src) => (Failure(fe(value)), src)

    def onFailure[E1 >: E, F, B](f: E1 => Result[F, B]): Parser[S, F, A | B] = 
        this(_) match
            case (Success(value), src) => (Success(value), src)
            case (Failure(value), src) => (f(value), src)

    def flatMap[B, F](f: A => Parser[S, F, B]): Parser[S, E | F, B] =
        this(_) match
            case (Success(value), src) => f(value)(src)
            case (Failure(value), src) => (Failure(value), src)

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

    def orElse[F, B](that: => Parser[S, F, B]): Parser[S, F, A | B] = src =>
        this(src) match
            case (Success(value), src) => (Success(value), src)
            case (Failure(value), _)   => that(src)

    def or[E1 >: E, F, B](that: => Parser[S, F, B])(using order: Ordering[E1 | F]): Parser[S, E | F, A | B] = src =>
        this(src) match
            case (Success(value), src1) => (Success(value), src1)
            case (Failure(e), src1)   => that(src) match
                case (Success(value), src2) => (Success(value), src2)
                case (Failure(f), src2)   => if order.gt(e, f) then (Failure(e), src1) else (Failure(f), src1)

    def repeat: Parser[S, Nothing, List[A]] = (
        for head <- this
            tail <- repeat
        yield head :: tail) orElse Parser[S].success(List.empty)

    def alternate[F, B](that: Parser[S, F, B]): Parser[S, F, List[(A, B)]] =
        this(_) match
            case (Success(a), src) => (
                for b    <- that
                    rest <- this alternate that
                yield (a, b) :: rest)(src)
            case (Failure(e), src) => (Success(List.empty), src)

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
