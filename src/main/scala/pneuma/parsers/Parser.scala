package pneuma.parsers

import Parser.Result.{Success, Failure}
import Parser.Result

object Parser {

    enum Result[+E, +A] {
        case Success(value: A)
        case Failure(value: E)

        def onFailure[F, B](f: E => Result[F, B]) = this match
            case Success(value) => Success(value)
            case Failure(value) => f(value)
    }

    def from[S, A](f: S => (A, S)): Parser[S, Nothing, A] = (src0: S) =>
        val (result, src1) = f(src0)
        (Success(result), src1)

    def apply[S] = new Creator[S]

    class Creator[S] {
        def success[A](value: => A): Parser[S, Nothing, A] = (src: S) => (Success(value), src)
        def failure[E](value: => E): Parser[S, E, Nothing] = (src: S) => (Failure(value), src)
        def result[E, A](value: => Result[E, A]): Parser[S, E, A] = (src: S) => (value, src)
    }

}

trait Parser[S, +E, +A] extends (S => (Result[E, A], S)) {

    def map[B](f: A => B): Parser[S, E, B] =
        this(_) match
            case (Success(value), src) => (Success(f(value)), src)
            case (Failure(value), src) => (Failure(value), src)

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

    def or[F, B](that: => Parser[S, F, B]): Parser[S, F, A | B] = (src: S) =>
        this(src) match
            case (Success(value), src) => (Success(value), src)
            case (Failure(value), _)   => that(src)

    def priorizedOr[E1 >: E, F, B](that: => Parser[S, F, B])(using order: Ordering[E1 | F]): Parser[S, E | F, A | B] = (src: S) =>
        this(src) match
            case (Success(value), src1) => (Success(value), src1)
            case (Failure(e), src1)   => that(src) match
                case (Success(value), src2) => (Success(value), src2)
                case (Failure(f), src2)   => if order.gt(e, f) then (Failure(e), src1) else (Failure(f), src1)
        // val (res1, src1) = this(src0)
        // (res1.onFailure(e => 
        //     val (res2, src2) = that(src0)
        //     res2.onFailure(f =>
        //         Failure(order.max(e, f))
        //     )
        // ), src1)

    def rep: Parser[S, Nothing, List[A]] = (
        for head <- this
            tail <- rep
        yield head :: tail) or Parser[S].success(List.empty)

    def alternating[F, B](that: Parser[S, F, B]): Parser[S, F, List[(A, B)]] =
        this(_) match
            case (Success(a), src) => (
                for b    <- that
                    rest <- this alternating that
                yield (a, b) :: rest)(src)
            case (Failure(e), src) => (Success(List.empty), src)

    def fold[A1 >: A](op: (A1, A1) => A1): Parser[S, E, A1] =
        for head <- this
            tail <- this.rep
        yield tail.foldLeft(head)(op)

    def foldsep[F, B, A1 >: A](that: Parser[S, F, B])(op: (A1, B, A1) => A1): Parser[S, E, A1] =
        for head <- this
            tail <- that alternating this
        yield tail.foldLeft[A1](head) { case (a0, (b, a1)) => op(a0, b, a1)}

    // imperative foldsep

}
