package AbstractInterpreter

import ADT._
import AbstractInterpreter.Alias.{RunQueue, Store}
import cats._

import scala.collection.immutable.HashMap

package object Alias {

  //The store is a finite partial mapping from Channels to ChannelQueues.
  type Store = HashMap[Channel,ChannelQueue]

  //The run-queue is just the list of expressions to be evaluated.
  type RunQueue = List[Proc[Channel]]

}

// When creating sample expressions, every name must be unique!
object Example extends App {

  // @0!0
  val reducible_1 = List(Output(Quote(Zero()), Par(Zero(), Zero())))

  // @0!(0|0)
  val reducible_2 = List(Output(Quote(Zero()), Par(Zero(), Zero())))

  // @(0|0)!0
  val reducible_3 = List(Output(Quote(Par(Zero(), Zero())), Zero()))

  // @(0|0)!(0|0)
  val reducible_4 = List(Output(Quote(Par(Zero(), Zero())), Par(Zero(), Zero()))) // counter example

  // @0!(*@(0|0))
  val reducible_5 = List(Output(Quote(Zero()), Drop(Quote(Par(Zero(), Zero())))))

  // @0!(*@(0|0)) | for(@(0|0|0) <- @0){ *@(0|0|0)!( }
  val reducible_6 = List(
    Par(
      Output(
        Quote(Zero()),
        Drop(Quote(Par(Zero(), Zero())))
      ),
      Input(
        Quote(Par(Zero(), Zero(), Zero())),
        Quote(Zero()),
        Drop(Quote(Par(Zero(), Zero(), Zero())))
      ),
    )
  )

  // new x in { x!(0) }
  val reducible_7 = List(
    New(
      Var("x"),
      Output(
        Var("x"),
        Zero()
      )
    )
  )

  // new x in { x!(0) | for(z <- x){*z} }
  val reducible_8 = List(
    New[Channel](
      Var("x"),
      Par(
        Output(
          Var("x"),
          Zero()
        ),
        Input(
          Var("z"),
          Var("x"),
          Drop(Var("z"))
        )
      )
    )
  )

  // new x in { x!(0) | for(z <- x){ *z } }
  val reducible_9 = List(
    New(
      Var("x"),
      Par(
        Output(
          Var("x"),
          Zero()
        ),
        Input(
          Var("z"),
          Var("x"),
          Drop(Var("z"))
        ),
        Input(
          Var("u"),
          Var("x"),
          Drop(Var("u"))
        )
      )
    )
  )

  // new x in { new y in { x!(*y) | for(u <- y){ u!0 }} | for( z <- x ){ z!(0) } }
  val reducible_10 = List(
    New(
      Var("x"),
      Par(
        New(
          Var("y"),
          Par(
            Output(Var("x"), Drop(Var("y"))),
            Input(Var("u"), Var("y"), Output(Var("u"), Zero()))
          )
        ),
        Input(Var("z"), Var("x"), Output(Var("z"), Zero()))
      )
    )
  )

  // new x in { @(0|0)!*x | for(z <- @0){ new y in { z!(y!(0)) | for(v <- y){ *v } | for(q <- z){ *q }}}}

  val reducible_11 = List(
    New[Channel](
     Var("x"),
      Par(
        Output(Quote(Par(Zero(),Zero())),Drop(Var("x"))),
        Input(
          Var("z"),
          Quote(Par(Zero(),Zero())),
          New(
            Var("y"),
            Par(
              Output(Var("z"),Output(Var("y"),Zero())),
              Input(Var("v"),Var("y"),Drop(Var("v"))),
              Input(Var("q"),Var("z"),Drop(Var("q")))
            )
          )
        )
      )
    )
  )

  val result = RhoInterface.reduceM.reduce(MachineState(HashMap.empty[Channel, ChannelQueue], reducible_11))

  println(result.length)
  println(result.map{triple => triple._3.mkString("\n")}.mkString("\n" + "Terminated" + "\n" + "\n"))
}

//A channel may be a quoted process, or a variable.
sealed trait Channel

  case class Quote(unquote: Proc[Channel]) extends Channel {
    override def toString: String = "@(" + unquote + ")"
  }

  case class Var(id: String) extends Channel {
    override def toString: String = id
  }

//A ChannelQueue may be an empty queue, a list of readers, or a list of writers.
// It will never be both a list of readers and a list of writers
sealed trait ChannelQueue

  case class ReaderQueue(x:Reader, xs: List[Reader]) extends ChannelQueue {
    override def toString: String = (x :: xs).map{_.toString}.mkString("[", "][", "]")
  }

  case class WriterQueue(x: Writer, xs: List[Writer]) extends ChannelQueue {
    override def toString: String = (x :: xs).map{_.toString}.mkString("[", "][", "]")
  }

  case class EmptyQueue() extends ChannelQueue {
    override def toString: String = "[]"
  }

//A reader is a abstraction providing a bound name "z", and a continuation
//to evaluate, "k".
sealed trait Reader

  case class Abstraction(z: Channel, k: Proc[Channel]) extends Reader {
    override def toString: String = " λ" + z + " { " + k.toString + " } "
  }

//A writer simply represents a message, "q".
sealed trait Writer

  case class Concretion(q: Channel) extends Writer {
    override def toString: String = " " + q.toString + " "
  }


case class MachineState(store: Store, runQueue: RunQueue){
  override def toString: String = "Queue: " + runQueue.mkString(" | ") + "\n" + "Store: " + store.mkString("{ ",", "," }") + "\n"
}

//ReduceM := StateT[WriterT[List,List,(S,A)]] = S => WriterT[List,List,(S,A)]] = S => (S,A) => List[(List[S],(S,A)] = List[(A,S,List[S])]

//Parametric on notion of State, S, and the value being returned, A.
case class ReduceM[S,A](reduce: S => List[(A, S, List[S])]) {

  def withFilter(pred: A => Boolean): ReduceM[S,A] = ReduceM {
    state =>
      for { triple <- reduce(state) ; if pred(triple._1) } yield triple
  }

  def map[B](f: A => B): ReduceM[S,B] = ReduceM {
    state0 =>
      for { (ret,st1,log) <- reduce(state0) } yield {
        (f(ret),st1,log)
      }
  }

  def flatMap[B](f: A => ReduceM[S, B]): ReduceM[S, B] = ReduceM {
    state0 =>
      for { (a, state1, log0) <- reduce(state0)
            (b, state2, log1) <- f(a).reduce(state1) } yield {
        (b, state2, log0 ++ log1)
      }
  }
}

object ReduceM {

  def get[S]: ReduceM[S, S] = {
    state { st => (st, st) }
  }

  def put[S]: S => ReduceM[S, Unit] = {
    state1 =>
      state { state0 => ((), state1) }
  }

  def tell[S]: List[S] => ReduceM[S, Unit] =
    log =>
      writer((), log)

  def listen[S, A](ma: ReduceM[S, A]): ReduceM[S, (A, List[S])] = ReduceM {
    state0 =>
      for {(ret, state1, log) <- ma.reduce(state0)} yield {
        ((ret, log), state1, log)
      }
  }

  def fromList[S, A](xs: List[A]): ReduceM[S, A] = ReduceM {
    state =>
      xs map { x => (x, state, List()) }
  }

  def writer[S, A](entry: (A, List[S])): ReduceM[S, A] = ReduceM {
    state =>
      List {
        (entry._1, state, entry._2)
      }
  }

  def state[S, A](f: S => (A, S)): ReduceM[S, A] = ReduceM {
    state0 =>
      f(state0) match {
        case (ret, state1) => List {
          (ret, state1, Nil)
        }
      }
  }

  //Insight on how to more idiomatically define this type-class is appreciated.
  implicit def reduceMonad[S]: Monad[ReduceM[S, ?]] = {

    new Monad[ReduceM[S, ?]] {

      def pure[A](x: A): ReduceM[S, A] = ReduceM {
        state =>
          List {(x, state, Nil)}
      }

      def flatMap[A, B](ma: ReduceM[S, A])(f: A => ReduceM[S, B]): ReduceM[S, B] = ma.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => ReduceM[S, Either[A, B]]): ReduceM[S, B] = {
        def go(e: Either[A, B]): ReduceM[S, B] = e match {
          case Left(a1) => tailRecM(a1)(f)
          case Right(b) => pure[B](b)
        }
        f(a).flatMap(go)
      }
    }
  }
}


object RhoInterface {


  val getStore: ReduceM[MachineState, Store] =
    ReduceM.state { st => (st.store, st) }


  val putStore: Store => ReduceM[MachineState, Unit] =
    store =>
      ReduceM.state { st0 => ((), MachineState(store, st0.runQueue)) }


  val getRunQueue: ReduceM[MachineState, RunQueue] =
    ReduceM.state { st => (st.runQueue, st) }


  val putRunQueue: RunQueue => ReduceM[MachineState, Unit] =
    runQ =>
      ReduceM.state { st0 => ((), MachineState(st0.store, runQ)) }

  // cancel(@*N) = N
  val cancel: Channel => Channel = {
    case Quote(Drop(n)) => n
    case chan => chan
  }

  // Variable binding is represented by substitution in the body of the process.
  // A more efficient implementation will achieve the same result using environments.
  val bind: Channel => Channel => Proc[Channel] => Proc[Channel] =
    chan0 =>
      chan1 =>
        proc =>
          Proc.functorProc.map[Channel,Channel](proc) { name =>
            //@*@Q = @Q in P{@*@Q/z}
            if (name.equals(chan0)) cancel(chan1)
            else name
          }


  val write: Channel => ChannelQueue => ReduceM[MachineState, Unit] =
    chan =>
      chanQ =>
        for {store <- getStore
             _ <- putStore(store + {
               chan -> chanQ
             })
        } yield ()


  val alloc: Channel => ReduceM[MachineState, ChannelQueue] = {
    chan =>
      val e = EmptyQueue()
      for {_ <- write(chan)(e)} yield e
  }


  val read: Channel => ReduceM[MachineState, ChannelQueue] =
    chan =>
      for {store <- getStore
           chanQ1 <- store.get(chan) match {
             //I'd like to just be able to say pure(chanQ0)
             case Some(chanQ0) => ReduceM.reduceMonad[MachineState].pure(chanQ0)
             case None => alloc(chan)
           }
      } yield chanQ1


  val readerQueue: List[Reader] => ChannelQueue = {
    case Nil => EmptyQueue()
    case reader :: rs => ReaderQueue(reader, rs)
  }


  val writerQueue: List[Writer] => ChannelQueue = {
    case Nil => EmptyQueue()
    case writer :: ws => WriterQueue(writer, ws)
  }


  val reduceM: ReduceM[MachineState, Unit] = {

    // Get run-queue and pull first process off.
    for {runQueue <- getRunQueue

         _ <- runQueue match {

           // If the queue is empty, log the final state, and terminate.
           case Nil =>

             for {st <- ReduceM.get[MachineState]

                  _ <- ReduceM.tell(List(st))} yield ()

           case proc :: xs =>

             for {_ <- proc match {

                 // (Store, 0 :: R) -> (Store, R)
               case Zero() =>

                 for {st <- ReduceM.get[MachineState]

                      _ <- ReduceM.tell(List(st))

                      _ <- putRunQueue(xs)

                 } yield ()


               case par@Par(_*) =>

                 // Encodes non-determinism by generating an auxiliary run-queue for every permutation of the set (P1 | ... | Pn)
                 for {interleaving <- ReduceM.fromList(par.processes.permutations.toList)

                      // Adds a permutation to the original run-queue
                      newRunQueue = (interleaving ++ xs).toList

                      // Continues evaluating with new run-queue
                      _ <- putRunQueue(newRunQueue)

                 } yield ()

               case Input(z, x, k) =>

                 val abs = Abstraction(z, k)

                 for {st <- ReduceM.get[MachineState]

                      _ <- ReduceM.tell(List(st))

                      chanQ <- read(x)

                      _ <- chanQ match {

                          // If there is a writer waiting, pull it off, and bind it's message to z in k.
                        case WriterQueue(writer: Concretion, writers) =>

                          for {_ <- write(x)(writerQueue(writers))

                               _ <- putRunQueue(bind(z)(writer.q)(k) :: xs)

                          } yield ()

                          // If there is a reader waiting, create a reader, Abstraction(z,k), and add to the end of queue.
                        case ReaderQueue(reader, readers) =>

                          for {_ <- write(x)(readerQueue((reader :: readers) :+ abs))

                               _ <- putRunQueue(xs)

                          } yield ()

                          // If queue is empty, create a ReaderQueue, and add reader to it.
                        case EmptyQueue() =>

                          for {_ <- write(x)(readerQueue(List(abs)))

                               newStore <- getStore

                               _ <- ReduceM.put(MachineState(newStore, xs))

                          } yield ()
                      }
                 } yield ()


               case Output(x, q) =>

                 val atQ = Quote(q)

                 for {st <- ReduceM.get[MachineState]

                      _ <- ReduceM.tell(List(st))

                      chanQ <- read(x)

                      _ <- chanQ match {

                          // Similar to ReaderQueue rule in Input.
                        case WriterQueue(writer: Concretion, writers) =>

                          for {_ <- write(x)(writerQueue((writer :: writers) :+ Concretion(atQ)))

                               newStore <- getStore

                               _ <- ReduceM.put(MachineState(newStore, xs))

                          } yield ()

                          // If reader in the queue, pull it off, bind message, and add continuation to the end of the run-queue
                        case ReaderQueue(reader, readers) =>

                          reader match {

                            case Abstraction(z, k) =>

                              for {_ <- write(x)(readerQueue(readers))

                                   newStore <- getStore

                                   _ <- ReduceM.put(MachineState(newStore, xs :+ bind(z)(atQ)(k)))

                              } yield ()
                          }

                          // Similar to EmptyQueue rule in Input
                        case EmptyQueue() =>

                          for {_ <- write(x)(writerQueue(List(Concretion(atQ))))

                               newStore <- getStore

                               _ <- ReduceM.put(MachineState(newStore, xs))

                          } yield ()
                      }
                 } yield ()

                //
               case Drop(x) =>

                 x match {

                   case Quote(p) =>

                     for {st <- ReduceM.get[MachineState]

                          _ <- ReduceM.tell(List(st))

                          //(Store, *@Q :: R) -> (Store, Q :: R)
                          _ <- putRunQueue(p :: xs)} yield ()

                   case v@Var(_) => sys.error(s"Variable $v cannot be de-referenced")
                 }


               case New(x, k) =>

                 for {st <- ReduceM.get[MachineState]

                      _ <- ReduceM.tell(List(st))

                      _ <- alloc(x)

                      _ <- putRunQueue(k :: xs)

                 } yield ()

               case _ => sys.error("Undefined term")
             }

                  _ <- reduceM

             } yield ()
         }
    } yield ()
  }
}
