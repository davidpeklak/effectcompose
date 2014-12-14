import scalaz.{Free, Scalaz, Monad}
import scalaz.Scalaz._
import scalaz.concurrent.Task

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

    val ListStateEffect = new StateEffect[List, Int] {
      implicit def MM: Monad[List] = MMList
    }

    import ListStateEffect._

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

  object StateEffectTask {

    val MMTask = implicitly[Monad[Task]] // I have to initialize that before using stateRun below

    val taskStateEffect = new StateEffect[Task, Int] {
      implicit def MM: Monad[Task] = MMTask
    }

    import taskStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        _ <- Task(println("Enter a number:")): FreeCT[Unit]
        i <- Task(readLine).map(_.toInt) : FreeCT[Int]
        j <- fGet
        _ <- fPut(i * j)
      } yield ()

    //// Interpretation

    val interpret = new StateEffectInterpret[Task, Int] {
      implicit def MM: Monad[Task] = MMTask
    }

    // run with Usage.StateEffectTask.stateRun(3).run
    lazy val stateRun = Free.runFC[StateEffect[Task, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)
  }

}
