import scalaz.{Free, Scalaz, Monad}
import scalaz.Scalaz._

object Usage {

  object StateEffectId {

    val MMId = implicitly[Monad[Id]] // I have to initialize that before using stateRun below

    val idStateEffect = new StateEffect[Id, Int] {
      implicit def MM: Monad[Id] = MMId
    }

    import idStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        i <- fGet
        _ <- fPut(i * 3)
      } yield ()

    //// Interpretation

    val interpret = new StateEffectInterpret[Id, Int] {
      implicit def MM: Monad[Id] = MMId
    }

    // run with Usage.StateEffectId.stateRun(3)
    lazy val stateRun = Free.runFC[StateEffect[Id, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)

  }

}
