import scalaz._
import scalaz.Free.FreeC
import scalaz.Scalaz._
import scalaz.concurrent.Task

object KastenT {

  def identTrans[M[_]]: (M ~> M) = new (M ~> M) {
    override def apply[A](fa: M[A]): M[A] = fa
  }

  /**
   * turns a transition F ~> M into a transition FreeC[F] ~> M
   */
  def freeCTrans[F[_], M[_]: Monad](t: (F ~> M)): (({type l[a] = FreeC[F, a]})#l ~> M) = new (({type l[a] = FreeC[F, a]})#l ~> M) {
    def apply[A](fa: FreeC[F, A]): M[A] = Free.runFC(fa)(t)
  }

  /////////////////////////
  // The State stuff as a MonadTransformer
  /////////////////////////

  /**
   * Compare to Kasten
   * @tparam M a Monad
   *
   *           As opposed to Kasten, where I just transformed to State,
   *           I want to transform to StateT[M] now, and also allow other
   *           operations of M
   */
  trait TransformEffState[M[_]] {

    implicit def MM: Monad[M]

    /**
     * Represents State operations that have the "effect" of operating on the S
     * @tparam S the state variable
     * @tparam A the result of an operation
     */
    sealed trait EffState[S, A]

    case class Get[S]() extends EffState[S, S]

    case class Put[S](s: S) extends EffState[S, Unit]

    /**
     * Represents something else than a state operation, namely na operation of the underlying monad
     * (i.e. the monad that we are going to stack the StateT onto)
     * @param ma the underlying monad operation
     * @tparam S the state variable
     * @tparam A the result of the operation
     */
    case class OtherState[S, A](ma: M[A]) extends EffState[S, A]

    /**
     * Fixes the type S of EffState to some concrete type and supplies functions and types for that specific S
     * @tparam S
     */
    trait EffStateFree[S] {

      type EffStateS[A] = EffState[S, A]

      type FreeCT[A] = FreeC[EffStateS, A]

      private type FreeFunctor[A] = Coyoneda[EffStateS, A]

      implicit val FreeCTM: Monad[FreeCT] = Free.freeMonad[FreeFunctor](Coyoneda.coyonedaFunctor[EffStateS])

      def ls[A](esa: EffStateS[A]): FreeCT[A] = {
        Free.liftFC[EffStateS, A](esa)
      }

      /**
       * lifts a monad M[A] directly to a FreeCT[A]. This is important because otherwise
       * we would need two conversions in a row, from M[A] to EffStateS[A] to FreeCT[A].
       * That would not work implictely.
       */
      implicit def lss[A](ma: M[A]): FreeCT[A] = {
        Free.liftFC[EffStateS, A](OtherState[S, A](ma))
      }

      def sGet: FreeCT[S] = ls(Get[S]())

      def sPut(s: S): FreeCT[Unit] = ls(Put[S](s))

      def sOtherState[A](ma: M[A]): FreeCT[A] = lss[A](ma)
    }

    trait EffStateInterpret[S] {

      type EffStateS[A] = EffState[S, A]

      type StateTS[A] = StateT[M, S, A]

      val STM = StateT.stateTMonadState[S, M]

      val transToState: (EffStateS ~> StateTS) = new (EffStateS ~> StateTS) {
        def apply[A](fa: EffState[S, A]): StateT[M, S, A] = fa match {
          case Get() => STM.get.asInstanceOf[StateT[M, S, A]]
          case Put(s) => STM.put(s).asInstanceOf[StateT[M, S, A]]
          case OtherState(ma) => StateT[M, S, A](s => ma.map(a => (s, a)))
        }
      }
    }

  }

  ///////////// For Id

  val transformEffStateId = new TransformEffState[Id] {
    def MM = implicitly[Monad[Id]]

    val effStateFreeInt = new EffStateFree[Int] {}

    import effStateFreeInt._

    val daStateFree: FreeCT[Unit] =
      for {
        _ <- sPut(3)
        i <- sGet
        _ <- sPut(i * 3)
      } yield ()

    val effStateInterpretInt = new EffStateInterpret[Int] {}

    import effStateInterpretInt._

    val daState = Free.runFC(daStateFree)(transToState)
  }

  ///////////// For List

  val transformEffStateList = new TransformEffState[List] {
    def MM = implicitly[Monad[List]]

    val effStateFreeInt = new EffStateFree[Int] {}

    import effStateFreeInt._

    val daStateFree: FreeCT[Int] =
      for {
        e <- List(1, 2, 3): FreeCT[Int]
        _ <- sPut(e)
        i <- sGet
        _ <- sPut(i * 3)
      } yield e

    val effStateInterpretInt = new EffStateInterpret[Int] {}

    import effStateInterpretInt._

    val daState = Free.runFC(daStateFree)(transToState)
  }

  ///////////// For Task, now we are talking...

  val transformEffStateTask = new TransformEffState[Task] {
    def MM = implicitly[Monad[Task]]

    val effStateFreeInt = new EffStateFree[Int] {}

    import effStateFreeInt._

    val daStateFree: FreeCT[Int] =
      for {
        e <- Task.delay(readLine).map(_.toInt): FreeCT[Int]
        i <- sGet
        _ <- sPut(i * e)
      } yield e

    val effStateInterpretInt = new EffStateInterpret[Int] {}

    import effStateInterpretInt._

    val daState = Free.runFC(daStateFree)(transToState)
  }

  /////////////////////////
  // The Exception stuff as a MonadTransformer
  /////////////////////////

  /**
   * @tparam F might be a free monad for which we need an interpreter to transform it into an M. It might also be M, then the transformation is identity
   */
  trait TransformEffException[F[_], M[_]] {

    implicit def MM: Monad[M]

    // abstract

    sealed trait EffException[E]

    case class Raise[E](e: E) extends EffException[E]

    case class OtherException[E, A](fa: F[A]) extends EffException[E]

    trait EffExceptionFree[E] {

      type EffExceptionE[A] = EffException[E] // A is simply ignored here

      type FreeCT[A] = FreeC[EffExceptionE, A]

      private type FreeFunctor[A] = Coyoneda[EffExceptionE, A]

      implicit val FreeCTM: Monad[FreeCT] = Free.freeMonad[FreeFunctor](Coyoneda.coyonedaFunctor[EffExceptionE])

      def le[A](ee: EffExceptionE[A]): FreeCT[A] = {
        Free.liftFC[EffExceptionE, A](ee)
      }

      implicit def lee[A](fa: F[A]): FreeCT[A] = {
        Free.liftFC[EffExceptionE, A](OtherException[E, A](fa))
      }

      def sRaise[A](e: E): FreeCT[A] = le(Raise(e))

      def sOtherException[A](fa: F[A]): FreeCT[A] = lee(fa)
    }

    trait EffExceptionInterpret[E] {
      type EffExceptionE[A] = EffException[E] // A is simply ignored here

      type OptionTE[A] = OptionT[M, A]

      def transToOption(FtoM: F ~> M): (EffExceptionE ~> OptionTE) = new (EffExceptionE ~> OptionTE) {
        override def apply[A](fa: EffExceptionE[A]): OptionTE[A] = fa match {
          case Raise(e) => OptionT.none[M, A]
          case OtherException(fa) => OptionT[M, A](FtoM(fa).map(a => Some(a.asInstanceOf[A])))
        }
      }

      type EitherTE[A] = EitherT[M, E, A]

      def transToEither(FtoM: F ~> M): (EffExceptionE ~> EitherTE) = new (EffExceptionE ~> EitherTE) {
        override def apply[A](fa: EffExceptionE[A]): EitherTE[A] = fa match {
          case Raise(e) => EitherT.left(MM.point(e))
          case OtherException(fa) => EitherT.right[M, E, A](FtoM(fa).asInstanceOf[M[A]])
        }
      }
    }

  }

  ///////////// For Id

  val transformedEffExceptionId = new TransformEffException[Id, Id] {
    def MM = implicitly[Monad[Id]]

    val effExceptionFreeString = new EffExceptionFree[String] {}

    import effExceptionFreeString._

    val daExceptionFree: FreeCT[Unit] =
      for {
        _ <- sRaise("Error!"): FreeCT[Unit]
      } yield ()

    val effExceptionInterpretString = new EffExceptionInterpret[String] {}

    import effExceptionInterpretString._

    lazy val daOption = Free.runFC(daExceptionFree)(transToOption(identTrans[Id]))

    lazy val daEither = Free.runFC(daExceptionFree)(transToEither(identTrans[Id]))
  }

  ///////////// For List

  val transformedEffExceptionList = new TransformEffException[List, List] {
    def MM = implicitly[Monad[List]]

    val effExceptionFreeString = new EffExceptionFree[String] {}

    import effExceptionFreeString._

    val daExceptionFree: FreeCT[Int] =
      for {
        i <- List(1, 2, 3): FreeCT[Int]
        j <- {
          if (i < 3) sRaise("Error!"): FreeCT[Int] else List(i): FreeCT[Int]
        }
      } yield j

    val effExceptionInterpretString = new EffExceptionInterpret[String] {}

    import effExceptionInterpretString._

    lazy val daOption = Free.runFC(daExceptionFree)(transToOption(identTrans[List]))

    lazy val daEither = Free.runFC(daExceptionFree)(transToEither(identTrans[List]))
  }

  ///////////// For Task

  val transformedEffExceptionTask = new TransformEffException[Task, Task] {
    def MM = implicitly[Monad[Task]]

    val effExceptionFreeString = new EffExceptionFree[String] {}

    import effExceptionFreeString._

    val daExceptionFree: FreeCT[Int] =
      for {
        i <- Task.delay(readLine).map(_.toInt): FreeCT[Int]
        j <- {
          if (i < 3) sRaise("Error!"): FreeCT[Int] else Task.now(i): FreeCT[Int]
        }
      } yield j

    val effExceptionInterpretString = new EffExceptionInterpret[String] {}

    import effExceptionInterpretString._

    lazy val daOption = Free.runFC(daExceptionFree)(transToOption(identTrans[Task]))

    lazy val daEither = Free.runFC(daExceptionFree)(transToEither(identTrans[Task]))
  }

  /////////////////////////
  // Attempt to wrap an ExceptionT around a StateT
  /////////////////////////

  ///////////// Id

  val transformEffStateIdSuperHengst = new TransformEffState[Id] {
    def MM = implicitly[Monad[Id]]

    val effStateFreeInt = new EffStateFree[Int] {}

    import effStateFreeInt.{lss, sGet, sPut, sOtherState, FreeCTM, EffStateS, FreeCT => FreeCTState}

    // we already need an interpreter for the inner Monad (the StateT) here, because the TransformEffException
    // needs to know what the target Monad M is.
    val effStateInterpretInt = new EffStateInterpret[Int] {}

    val transformEffExceptionSuperHengst = new TransformEffException[FreeCTState, effStateInterpretInt.StateTS] {
      def MM = effStateInterpretInt.STM

      val effExceptionFreeString = new EffExceptionFree[String] {}

      import effExceptionFreeString.{lee, sRaise, sOtherException, FreeCTM, FreeCT => FreeCTException}

      // i need an implicit convsersion directly from Id[A] to FreeCTException[A], so that I can also use Id[A] in the for loop
      implicit def lse[A](ma: A): FreeCTException[A] = lee(sOtherState(ma))

      type X = FreeCTException[Int]
      val daExceptionFree: X =
        for {
          i <- sGet: X
          j <- {
            if (i < 3) sRaise("Error!"): X else i: X
          }
        } yield j

      val effExceptionInterpretString = new EffExceptionInterpret[String] {}

      import effExceptionInterpretString._

      lazy val daOption = Free.runFC(daExceptionFree)(transToOption(new (FreeCTState ~> effStateInterpretInt.StateTS) {
        def apply[A](fa: FreeCTState[A]): effStateInterpretInt.StateTS[A] = Free.runFC(fa)(effStateInterpretInt.transToState)
      }))

      lazy val daEither = Free.runFC(daExceptionFree)(transToEither(new (FreeCTState ~> effStateInterpretInt.StateTS) {
        def apply[A](fa: FreeCTState[A]): effStateInterpretInt.StateTS[A] = Free.runFC(fa)(effStateInterpretInt.transToState)
      }))
    }
  }

  ///////////// List

  val transformEffStateListSuperHengst = new TransformEffState[List] {
    def MM = implicitly[Monad[List]]

    val effStateFreeInt = new EffStateFree[Int] {}

    import effStateFreeInt.{lss, sGet, sPut, sOtherState, FreeCTM, EffStateS, FreeCT => FreeCTState}

    // we already need an interpreter for the inner Monad (the StateT) here, because the TransformEffException
    // needs to know what the target Monad M is.
    val effStateInterpretInt = new EffStateInterpret[Int] {}

    val transformEffExceptionSuperHengst = new TransformEffException[FreeCTState, effStateInterpretInt.StateTS] {
      def MM = effStateInterpretInt.STM

      val effExceptionFreeString = new EffExceptionFree[String] {}

      import effExceptionFreeString.{lee, sRaise, sOtherException, FreeCTM, FreeCT => FreeCTException}

      // i need an implicit convsersion directly from Id[A] to FreeCTException[A], so that I can also use Id[A] in the for loop
      implicit def lse[A](ma: List[A]): FreeCTException[A] = lee(sOtherState(ma))

      type X = FreeCTException[Int]
      val daExceptionFree: X =
        for {
          i <- List(1, 2, 3, 4): X
          j <- sGet: X
          k <- {
            if (i < j) sRaise("Error!"): X else List(i): X
          }
        } yield k

      val effExceptionInterpretString = new EffExceptionInterpret[String] {}

      import effExceptionInterpretString._

      // run with KastenT.transformEffStateListSuperHengst.transformEffExceptionSuperHengst.daOption.run.apply(4)
      lazy val daOption = Free.runFC(daExceptionFree)(transToOption(new (FreeCTState ~> effStateInterpretInt.StateTS) {
        def apply[A](fa: FreeCTState[A]): effStateInterpretInt.StateTS[A] = Free.runFC(fa)(effStateInterpretInt.transToState)
      }))

      // run with KastenT.transformEffStateListSuperHengst.transformEffExceptionSuperHengst.daEither.run.apply(4)
      lazy val daEither = Free.runFC(daExceptionFree)(transToEither(new (FreeCTState ~> effStateInterpretInt.StateTS) {
        def apply[A](fa: FreeCTState[A]): effStateInterpretInt.StateTS[A] = Free.runFC(fa)(effStateInterpretInt.transToState)
      }))
    }
  }

  ///////////// Task

  val transformEffStateTaskSuperHengst = new TransformEffState[Task] {
    def MM = implicitly[Monad[Task]]

    val effStateFreeInt = new EffStateFree[Int] {}

    import effStateFreeInt.{lss, sGet, sPut, sOtherState, FreeCTM, EffStateS, FreeCT => FreeCTState}

    // we already need an interpreter for the inner Monad (the StateT) here, because the TransformEffException
    // needs to know what the target Monad M is.
    val effStateInterpretInt = new EffStateInterpret[Int] {}

    val transformEffExceptionSuperHengst = new TransformEffException[FreeCTState, effStateInterpretInt.StateTS] {
      def MM = effStateInterpretInt.STM

      val effExceptionFreeString = new EffExceptionFree[String] {}

      import effExceptionFreeString.{lee, sRaise, sOtherException, FreeCTM, FreeCT => FreeCTException}

      // i need an implicit conversion directly from Task[A] to FreeCTException[A], so that I can also use Task[A] in the for loop
      implicit def lse[A](ma: Task[A]): FreeCTException[A] = lee(sOtherState(ma))

      type X[A] = FreeCTException[A]

      // these are programs where I can freely mix Task, EffState and EffException expressions
      def Try[A, B](b: => B): X[B] =
        try {
          Task.now(b): X[B]
        }
        catch {
          case e: Exception => sRaise(e.toString): X[B]
        }

      val daExceptionFree: X[Int] =
        for {
          _ <- Task.delay(println("Enter a number: ")): X[Unit]
          s <- Task.delay(readLine): X[String]
          i <- Try(s.toInt)
          j <- sGet: X[Int]
          _ <- if (i < j) sRaise("Too small!"): X[Unit] else sPut(i): X[Unit]
          _ <- Task.delay(println("Success")): X[Unit] // note that this only happens if we don't sRaise above
        } yield i

      val effExceptionInterpretString = new EffExceptionInterpret[String] {}

      import effExceptionInterpretString._

      // run with KastenT.transformEffStateTaskSuperHengst.transformEffExceptionSuperHengst.daOption.run.apply(4).run
      lazy val daOption = Free.runFC(daExceptionFree)(transToOption(freeCTrans(effStateInterpretInt.transToState)))

      // run with KastenT.transformEffStateTaskSuperHengst.transformEffExceptionSuperHengst.daEither.run.apply(4).run
      lazy val daEither = Free.runFC(daExceptionFree)(transToEither(freeCTrans(effStateInterpretInt.transToState)))
    }
  }
}