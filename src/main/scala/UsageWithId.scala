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
        i <- 3 : FreeCT [Int]
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
        i <- 3 : FreeCT [Int]
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

}
