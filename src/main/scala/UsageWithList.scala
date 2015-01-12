import scalaz.Scalaz._
import scalaz.{~>, Free, Monad}

object UsageWithList {
  object StateEffectList {

    val MMList = implicitly[Monad[List]] // I have to initialize that before using stateRun below

    val listStateEffect = StateEffect[List, Int](MMList)

    import listStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        i <- List(1, 2, 3) : FreeCT[Int]
        j <- fGet
        _ <- fPut(i * j)
      } yield ()

    //// Interpretation

    val interpret = StateEffectInterpret[List, Int](MMList)

    // run with UsageWithList.StateEffectList.stateRun(3)
    lazy val stateRun = Free.runFC[StateEffect[List, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)
  }

  object ExceptionEffectList {
    val MMList = implicitly[Monad[List]]

    val listExceptionEffect = ExceptionEffect[List, String](MMList)

    import listExceptionEffect._

    val exceptionProgram: FreeCT[Int] =
      for {
        i <- List(1, 2, 3): FreeCT[Int]
        j <- if (i > 2) fRaise("Error!"): FreeCT[Int] else List(i): FreeCT[Int]
      } yield i

    //// Interpretation

    val interpret = ExceptionEffectInterpret[List, String](MMList)

    // run with UsageWithList.ExceptionEffectList.optionRun
    lazy val optionRun = Free.runFC[ExceptionEffect[List, String]#F, interpret.OptionTE, Int](exceptionProgram)(interpret.transToOption)

    // run with UsageWithList.ExceptionEffectList.eitherRun
    lazy val eitherRun = Free.runFC[ExceptionEffect[List, String]#F, interpret.EitherTE, Int](exceptionProgram)(interpret.transToEither)
  }
}
