import scalaz.{Free, Scalaz, Monad}
import scalaz.Scalaz._

class Usage {

  object StateEffectId {

    val idStateEffect = new StateEffect[Id, Int] {
      implicit def MM: Monad[Id] = implicitly[Monad[Id]]
    }

    import idStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        i <- fGet
        _ <- fPut(i * 3)
      } yield ()

    //// Interpretation

    val interpret = new StateEffectInterpret[Id, Int] {
      val stateEffect = idStateEffect
    }

    val stateRun = Free.runFC[EffState, interpret.StateTS, Unit](stateProgram)(interpret.transToState)

  }

}
