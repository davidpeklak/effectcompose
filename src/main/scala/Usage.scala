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
        i <- 3 : FreeCT [Int]
        j <- fGet
        _ <- fPut(i * j)
      } yield ()

    //// Interpretation

    val interpret = new StateEffectInterpret[Id, Int] {
      implicit def MM: Monad[Id] = MMId
    }

    // run with Usage.StateEffectId.stateRun(3)
    lazy val stateRun = Free.runFC[StateEffect[Id, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)
  }

  object StateEffectList {

    val MMList = implicitly[Monad[List]] // I have to initialize that before using stateRun below

    val idStateEffect = new StateEffect[List, Int] {
      implicit def MM: Monad[List] = MMList
    }

    import idStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        i <- List(1, 2, 3) : FreeCT[Int]
        j <- fGet
        _ <- fPut(i * j)
      } yield ()

    //// Interpretation

    val interpret = new StateEffectInterpret[List, Int] {
      implicit def MM: Monad[List] = MMList
    }

    // run with Usage.StateEffectList.stateRun(3)
    lazy val stateRun = Free.runFC[StateEffect[List, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)
  }

}
