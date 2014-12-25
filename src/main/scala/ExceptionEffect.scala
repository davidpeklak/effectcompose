import scalaz._

object ExceptionRepresentation {
  /**
   * Represents exception operations that have the "effect" of throwing an exception of type E
   * @tparam M the monad to stack the transformer on
   * @tparam E the exception type
   * @tparam A the result of an operation
   */
  sealed trait EffException[M[_], E, A]

  case class Raise[M[_], E, A](e: E) extends EffException[M, E, A]

  /**
   * Represents something else than an exception operation, namely an operation of the underlying monad
   * @param ma the underlying monad operation
   * @tparam A the result of the operation
   */
  case class OtherException[M[_], E, A](ma: M[A]) extends EffException[M, E, A]
}

/**
 * @tparam M the monad to stack the transformer on
 * @tparam E the exception type
 */
trait ExceptionEffect[M[_], E] extends Effect[M] {

  import ExceptionRepresentation._

  type F[A] = EffException[M, E, A]

  ///// Operations returning FreeCT

  def fRaise[A](e: E): FreeCT[A] = Free.liftFC[F, A](Raise[M, E, A](e))

  def fOther[A](ma: M[A]): F[A] = OtherException[M, E, A](ma)
}

trait ExceptionEffectInterpret[M[_], E] extends {

  implicit def MM: Monad[M] // abstract

  import ExceptionRepresentation._

  type F[A] = EffException[M, E, A]

  type OptionTE[A] = OptionT[M, A]

  val OTH = OptionT.optionTMonadTrans
  val OTM = OptionT.optionTMonadPlus[M]

  val transToOption: (F ~> OptionTE) = new (F ~> OptionTE) {
    def apply[A](fa: F[A]): OptionT[M, A] = fa match {
      case Raise(_) => OptionT.none[M, A]
      case OtherException(ma) => OTH.liftM(ma)
    }
  }

  type EitherTE[A] = EitherT[M, E, A]

  val ETH = EitherT.eitherTHoist[E]
  val ETM = EitherT.eitherTMonad[M, E]

  val transToEither: (F ~> EitherTE) = new (F ~> EitherTE) {
    def apply[A](fa: F[A]): EitherT[M, E, A] = fa match {
      case Raise(e) => EitherT.left[M, E, A](MM.point(e))
      case OtherException(ma) => ETH.liftM(ma)
    }
  }
}