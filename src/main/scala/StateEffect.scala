import scalaz._

/**
 * @tparam S the type of the state variable
 */
trait StateEffect[M[_], S] extends Effect[M] {

  ///// Representation

  /**
   * Represents State operations that have the "effect" of operating on the S
   * @tparam A the result of an operation
   */
  sealed trait EffState[A]

  type F[A] = EffState[A]

  case object Get extends EffState[S]

  case class Put(s: S) extends EffState[Unit]

  /**
   * Represents something else than a state operation, namely na operation of the underlying monad
   * (i.e. the monad that we are going to stack the StateT onto)
   * @param ma the underlying monad operation
   * @tparam A the result of the operation
   */
  case class OtherState[A](ma: M[A]) extends EffState[A]

  def fOther[A](ma: M[A]): F[A] = OtherState[A](ma)

  ///// Operations returning FreeCT

  def fGet: FreeCT[S] = Free.liftFC(Get)

  def fPut(s: S): FreeCT[Unit] = Free.liftFC(Put(s))
}

trait StateEffectInterpret[M[_], S] extends {

  val stateEffect: StateEffect[M, S] // abstract

  import stateEffect._

  type StateTS[A] = StateT[M, S, A]

  val STH = StateT.StateMonadTrans[S]
  val STM = StateT.stateTMonadState[S, M]

  val transToState: (EffState ~> StateTS) = new (EffState ~> StateTS) {
    def apply[A](fa: EffState[A]): StateT[M, S, A] = fa match {
      case Get => STM.get.asInstanceOf[StateT[M, S, A]]
      case Put(s) => STM.put(s).asInstanceOf[StateT[M, S, A]]
      case OtherState(ma) => STH.liftM(ma)
    }
  }
}

