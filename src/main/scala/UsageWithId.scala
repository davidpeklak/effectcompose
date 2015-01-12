import scalaz.{~>, Free, Scalaz, Monad}
import scalaz.Scalaz._

object UsageWithId {

  object StateEffectId {

    val MMId = implicitly[Monad[Id]] // I have to initialize that before using stateRun below

    val idStateEffect = StateEffect[Id, Int](MMId)

    import idStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        i <- 3: FreeCT[Int]
        j <- fGet
        _ <- fPut(i * j)
      } yield ()

    //// Interpretation

    val interpret = StateEffectInterpret[Id, Int](MMId)

    // run with UsageWithId.StateEffectId.stateRun(3)
    lazy val stateRun = Free.runFC[StateEffect[Id, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)
  }

  object ExceptionEffectId {
    val MMId = implicitly[Monad[Id]]

    val idExceptionEffect = ExceptionEffect[Id, String](MMId)

    import idExceptionEffect._

    val exceptionProgram: FreeCT[Int] =
      for {
        i <- 3: FreeCT[Int]
        _ <- fRaise[Unit]("Error!")
      } yield i

    //// Interpretation

    val interpret = ExceptionEffectInterpret[Id, String](MMId)

    // run with UsageWithId.ExceptionEffectId.optionRun
    lazy val optionRun = Free.runFC[ExceptionEffect[Id, String]#F, interpret.OptionTE, Int](exceptionProgram)(interpret.transToOption)

    // run with UsageWithId.ExceptionEffectId.eitherRun
    lazy val eitherRun = Free.runFC[ExceptionEffect[Id, String]#F, interpret.EitherTE, Int](exceptionProgram)(interpret.transToEither)
  }

  object ExceptionOnStateEffectId {
    val MMId = implicitly[Monad[Id]]

    val idStateEffect = StateEffect[Id, Int](MMId)

    val idExceptionEffect = ExceptionEffect[idStateEffect.FreeCT, String](idStateEffect.FreeCTM)

    import idStateEffect.{fGet, fPut}
    import idExceptionEffect._

    implicit def liftMTwice[A](a: A): FreeCT[A] = idExceptionEffect.liftM(idStateEffect.liftM(a))

    val stateExceptionProgram: FreeCT[Int] =
      for {
        i <- 3: FreeCT[Int]
        j <- fGet: FreeCT[Int]
        k <- if (i > j) i: FreeCT[Int] else fRaise("Too low!"): FreeCT[Int]
      }
      yield k

    //// Interpretation

    val interpretState = StateEffectInterpret[Id, Int](MMId)

    val interpretException = ExceptionEffectInterpret[idStateEffect.FreeCT, interpretState.StateTS, String](interpretState.STM, EffectCompose.freeCTrans(interpretState.transToState))

    // run with UsageWithId.ExceptionOnStateEffectId.optionRun.run.run(3)
    lazy val optionRun = Free.runFC[ExceptionEffect[idStateEffect.FreeCT, String]#F, interpretException.OptionTE, Int](stateExceptionProgram)(interpretException.transToOption)
  }

  object StateOnExceptionEffectId {
    val MMId = implicitly[Monad[Id]]

    val idExceptionEffect = ExceptionEffect[Id, String](MMId)

    val idStateEffect = StateEffect[idExceptionEffect.FreeCT, Int](idExceptionEffect.FreeCTM)

    import idExceptionEffect.fRaise
    import idStateEffect._

    implicit def liftMTwice[A](a: A): FreeCT[A] = idStateEffect.liftM(idExceptionEffect.liftM(a))

    val exceptionStateProgram: FreeCT[Int] =
      for {
        i <- 3: FreeCT[Int]
        j <- fGet: FreeCT[Int]
        k <- if (i > j) i: FreeCT[Int] else fRaise[Int]("Too low!"): FreeCT[Int]
      }
      yield k

    //// Interpretation

    val interpretException = ExceptionEffectInterpret[Id, String](MMId)

    val interpretState = StateEffectInterpret[idExceptionEffect.FreeCT, interpretException.OptionTE, Int](interpretException.OTM, EffectCompose.freeCTrans(interpretException.transToOption))

    // run with UsageWithId.StateOnExceptionEffectId.stateRun.run(2).run
    lazy val stateRun =  Free.runFC[StateEffect[idExceptionEffect.FreeCT, Int]#F, interpretState.StateTS, Int](exceptionStateProgram)(interpretState.transToState)

  }
}
