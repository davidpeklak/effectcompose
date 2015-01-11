import scalaz.concurrent.Task
import scalaz.{~>, Free, Monad}

object UsageWithTask {
  object StateEffectTask {

    val MMTask = implicitly[Monad[Task]] // I have to initialize that before using stateRun below

    val taskStateEffect = StateEffect[Task, Int](MMTask)

    import taskStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        _ <- Task(println("Enter a number:")): FreeCT[Unit]
        i <- Task(readLine).map(_.toInt) : FreeCT[Int]
        j <- fGet
        _ <- fPut(i * j)
      } yield ()

    //// Interpretation

    val interpret = new StateEffectInterpret[Task, Task, Int] {
      implicit def RM: Monad[Task] = MMTask

      override def MtoR: Task ~> Task = EffectCompose.identTrans
    }

    // run with UsageWithTask.StateEffectTask.stateRun(3).run
    lazy val stateRun = Free.runFC[StateEffect[Task, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)
  }

  object ExceptionEffectTask {
    val MMTask = implicitly[Monad[Task]]

    val taskExceptionEffect = new ExceptionEffect[Task, String] {
      implicit def MM: Monad[Task] = MMTask
    }

    import taskExceptionEffect._

    val exceptionProgram: FreeCT[Int] =
      for {
        i <- Task(readLine).map(_.toInt): FreeCT[Int]
        j <- if (i > 2) fRaise("Too high!"): FreeCT[Int] else Task.now(i): FreeCT[Int]
      } yield i

    //// Interpretation

    val interpret = new ExceptionEffectInterpret[Task, Task, String] {
      implicit def RM: Monad[Task] = MMTask

      def MtoR: Task ~> Task = EffectCompose.identTrans
    }

    // run with UsageWithTask.ExceptionEffectTask.optionRun.run.run
    lazy val optionRun = Free.runFC[ExceptionEffect[Task, String]#F, interpret.OptionTE, Int](exceptionProgram)(interpret.transToOption)

    // run with UsageWithTask.ExceptionEffectTask.eitherRun.run.run
    lazy val eitherRun = Free.runFC[ExceptionEffect[Task, String]#F, interpret.EitherTE, Int](exceptionProgram)(interpret.transToEither)
  }
}
