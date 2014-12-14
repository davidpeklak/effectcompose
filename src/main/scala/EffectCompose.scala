import scalaz._
import scalaz.Free.FreeC

object EffectCompose {

  def identTrans[M[_]]: (M ~> M) = new (M ~> M) {
    override def apply[A](fa: M[A]): M[A] = fa
  }

  /**
   * turns a transformation F ~> M into a transformation FreeC[F] ~> M
   */
  def freeCTrans[F[_], M[_]: Monad](t: (F ~> M)): (({type l[a] = FreeC[F, a]})#l ~> M) = new (({type l[a] = FreeC[F, a]})#l ~> M) {
    def apply[A](fa: FreeC[F, A]): M[A] = Free.runFC(fa)(t)
  }
}

/**
 * @tparam M a Monad to stack a Monadtransformer on
 */
trait Effect[M[_]] {
  implicit def MM: Monad[M] // abstract

  type F[_] // abstract

  def fOther[A](ma: M[A]): F[A] // abstract

  type FreeCT[A] = FreeC[F, A]

  private type FreeFunctor[A] = Coyoneda[F, A]

  implicit val FreeCTM: Monad[FreeCT] = Free.freeMonad[FreeFunctor](Coyoneda.coyonedaFunctor[F])

  /**
   * lifts a monad M[A] directly to a FreeCT[A]. This is important because otherwise
   * we would need two conversions in a row, from M[A] to EffStateS[A] to FreeCT[A].
   * That would not work implictely.
   */
  implicit def liftM[A](ma: M[A]): FreeCT[A] = {
    Free.liftFC[F, A](fOther[A](ma))
  }
}
