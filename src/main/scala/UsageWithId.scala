import scalaz.{~>, Free, Scalaz, Monad}
import scalaz.Scalaz._

object UsageWithId {

  object StateEffectId {

    val MMId = implicitly[Monad[Id]] // I have to initialize that before using stateRun below

    val idStateEffect = new StateEffect[Id, Int] {
      implicit def MM: Monad[Id] = MMId
    }

    import idStateEffect._

    val stateProgram: FreeCT[Unit] =
      for {
        i <- 3: FreeCT[Int]
        j <- fGet
        _ <- fPut(i * j)
      } yield ()

    //// Interpretation

    val interpret = new StateEffectInterpret[Id, Int] {
      implicit def MM: Monad[Id] = MMId
    }

    // run with UsageWithId.StateEffectId.stateRun(3)
    lazy val stateRun = Free.runFC[StateEffect[Id, Int]#F, interpret.StateTS, Unit](stateProgram)(interpret.transToState)
  }

  object ExceptionEffectId {
    val MMId = implicitly[Monad[Id]]

    val idExceptionEffect = new ExceptionEffect[Id, String] {
      implicit def MM: Monad[Id] = MMId
    }

    import idExceptionEffect._

    val exceptionProgram: FreeCT[Int] =
      for {
        i <- 3: FreeCT[Int]
        _ <- fRaise[Unit]("Error!")
      } yield i

    //// Interpretation

    val interpret = new ExceptionEffectInterpret[Id, Id, String] {
      implicit def RM: Monad[Id] = MMId

      def MtoR: Id ~> Id = EffectCompose.identTrans
    }

    // run with UsageWithId.ExceptionEffectId.optionRun
    lazy val optionRun = Free.runFC[ExceptionEffect[Id, String]#F, interpret.OptionTE, Int](exceptionProgram)(interpret.transToOption)

    // run with UsageWithId.ExceptionEffectId.eitherRun
    lazy val eitherRun = Free.runFC[ExceptionEffect[Id, String]#F, interpret.EitherTE, Int](exceptionProgram)(interpret.transToEither)
  }

  object ExceptionOnStateEffectId {
    val MMId = implicitly[Monad[Id]]

    val idStateEffect = new StateEffect[Id, Int] {
      implicit def MM: Monad[Id] = MMId
    }

    val idExceptionEffect = new ExceptionEffect[idStateEffect.FreeCT, String] {
      implicit def MM: Monad[idStateEffect.FreeCT] = idStateEffect.FreeCTM
    }

    import idStateEffect.{fGet, fPut}
    import idExceptionEffect._

    implicit def liftMTwice[A](a: A): FreeCT[A] = idExceptionEffect.liftM(idStateEffect.liftM(a))

    val stateExceptionProgram: FreeCT[Int] =
      for {
        i <- 3: FreeCT[Int]
        j <- fGet: FreeCT[Int]
        k <- if (i > j) i: FreeCT[Int] else fRaise("Too low!")
      }
      yield k

    //// Interpretation

    val interpretState = new StateEffectInterpret[Id, Int] {
      override implicit def MM: Monad[Id] = MMId
    }

    val interpretException = new ExceptionEffectInterpret[idStateEffect.FreeCT, interpretState.StateTS, String] {
      implicit def RM: Monad[interpretState.StateTS] = interpretState.STM

      def MtoR: idStateEffect.FreeCT ~> interpretState.StateTS = EffectCompose.freeCTrans(interpretState.transToState)
    }

    // run with UsageWithId.ExceptionOnStateEffectId.optionRun.run.run(3)
    lazy val optionRun = Free.runFC[ExceptionEffect[idStateEffect.FreeCT, String]#F, interpretException.OptionTE, Int](stateExceptionProgram)(interpretException.transToOption)

  }

}
