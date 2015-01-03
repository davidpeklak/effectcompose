import scalaz._

object StateRepresentation {
  /**
   * Represents state operations that have the "effect" of operating on the S
   * @tparam M the monad to stack the transformer on
   * @tparam S the type of the state variable
   * @tparam A the result of an operation
   */
  sealed trait EffState[M[_], S, A]

  case class Get[M[_], S]() extends EffState[M, S, S]

  case class Put[M[_], S](s: S) extends EffState[M, S, Unit]

  /**
   * Represents something else than a state operation, namely an operation of the underlying monad
   * (i.e. the monad that we are going to stack the StateT onto)
   * @param ma the underlying monad operation
   * @tparam A the result of the operation
   */
  case class OtherState[M[_], S, A](ma: M[A]) extends EffState[M, S, A]
}

/**
 * @tparam M the monad to stack the transformer on
 * @tparam S the type of the state variable
 */
trait StateEffect[M[_], S] extends Effect[M] {

  import StateRepresentation._

  type F[A] = EffState[M, S, A]

  ///// Operations returning FreeCT

  def fGet: FreeCT[S] = Free.liftFC[F, S](Get[M, S]())

  def fPut(s: S): FreeCT[Unit] = Free.liftFC[F, Unit](Put[M, S](s))

  def fOther[A](ma: M[A]): F[A] = OtherState[M, S, A](ma)
}

trait StateEffectInterpret[M[_], S] extends {

  implicit def MM: Monad[M] // abstract

  import StateRepresentation._

  type F[A] = EffState[M, S, A]

  type StateTS[A] = StateT[M, S, A]

  val STH = StateT.StateMonadTrans[S]
  val STM = StateT.stateTMonadState[S, M]

  val transToState: (F ~> StateTS) = new (F ~> StateTS) {
    def apply[A](fa: F[A]): StateT[M, S, A] = fa match {
      case g: Get[M, S] => STM.get.asInstanceOf[StateT[M, S, A]] // I can only check for the class here or the compiler shouts at me
      case Put(s) => STM.put(s).asInstanceOf[StateT[M, S, A]]
      case OtherState(ma) => STH.liftM(ma)
    }
  }
}

