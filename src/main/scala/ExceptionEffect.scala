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

object ExceptionEffect {
  def apply[M[_], E](mm: Monad[M]): ExceptionEffect[M, E] = new ExceptionEffect[M, E] {
    implicit def MM: Monad[M] = mm
  }
}

trait ExceptionEffectInterpret[M[_], R[_], E] extends {

  implicit def RM: Monad[R] // abstract

  def MtoR: M ~> R // abstract, should have default implementation for M == R

  import ExceptionRepresentation._

  type F[A] = EffException[M, E, A]

  type OptionTE[A] = OptionT[R, A]

  val OTH = OptionT.optionTMonadTrans
  val OTM = OptionT.optionTMonadPlus[R]

  def transToOption: (F ~> OptionTE) = new (F ~> OptionTE) {
    override def apply[A](fa: F[A]): OptionTE[A] = fa match {
      case Raise(e) => OptionT.none[R, A]
      case OtherException(ma) => OTH.liftM(MtoR(ma))
    }
  }

  type EitherTE[A] = EitherT[R, E, A]

  val ETH = EitherT.eitherTHoist[E]

  val transToEither: (F ~> EitherTE) = new (F ~> EitherTE) {
    def apply[A](fa: F[A]): EitherT[R, E, A] = fa match {
      case Raise(e) => EitherT.left[R, E, A](RM.point(e))
      case OtherException(ma) => ETH.liftM(MtoR(ma))
    }
  }
}

object ExceptionEffectInterpret {
  def apply[M[_], E](mm : Monad[M]): ExceptionEffectInterpret[M, M, E] = new ExceptionEffectInterpret[M, M, E] {
    def MtoR: M ~> M = EffectCompose.identTrans

    implicit def RM: Monad[M] = mm
  }

  def apply[M[_], R[_], E](rm : Monad[R], mtor: M ~> R): ExceptionEffectInterpret[M, R, E] = new ExceptionEffectInterpret[M, R, E] {
    implicit def RM: Monad[R] = rm

    def MtoR: M ~> R = mtor
  }
}