package general

object Result {
    def succeed[A](v: A) = Result.Success(v)
    def fail[E](e: E) = Result.Failure(Seq(e))
    def fail[E] = Result.Failure(Seq.empty[E])
    extension [E, A](self: Seq[Result[E, A]]) {
        def acc = self.foldLeft(Result.Success[E, Seq[A]](Seq.empty)) {
            case (Result.Success(res), Result.Success(elem)) => Result.Success(res :+ elem)
            case (Result.Failure(res), Result.Success(elem)) => Result.Failure(res)
            case (Result.Success(res), Result.Failure(elem)) => Result.Failure(elem)
            case (Result.Failure(res), Result.Failure(elem)) => Result.Failure(res ++ elem)
        }
    }
}

enum Result[+E, +A] {
    case Success(value: A)
    case Failure(values: Seq[E])
    def map[B](f: A => B): Result[E, B] = this match
        case Success(value) => Success(f(value))
        case Failure(values) => Failure(values)
    def flatMap[B, F](f: A => Result[F, B]): Result[E | F, B]  = this match
        case Success(value) => f(value)
        case Failure(values) => Failure(values)
    def foreach[B](f: A => Unit): Unit = this match
        case Success(value) => f(value)
        case _ =>
    def withFilter(f: A => Boolean) = this match
        case Success(value) if f(value) => Success(value)
        case Success(value) => Failure(Seq.empty)
        case Failure(values) => Failure(values)
    def toEither: Either[Seq[E], A] = this match
        case Success(value) => Right(value)
        case Failure(values) => Left(values)
    def toOption: Option[A] = this match
        case Success(value) => Some(value)
        case _ => None
    def transform[B, F](f: A => B, g: E => F) = this match
        case Success(value) => Success(f(value))
        case Failure(values) => Failure(values.map(g))
}
