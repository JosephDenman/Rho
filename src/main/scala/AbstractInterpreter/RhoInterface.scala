package AbstractInterpreter

import ADT.Proc._
import ADT.{Channel, Proc, Quote, Var}
import Alias._
import cats.implicits._

import scala.collection.immutable.HashMap


// When creating sample expressions, every name must be unique!
object Example extends App {

  // @0!0
  val reducible_1 = Output(Quote(Zero()), Zero())

  // @0!(0|0)
  val reducible_2 = Output(Quote(Zero()), Par(Zero(), Zero()))

  // @(0|0)!0
  val reducible_3 = Output(Quote(Par(Zero(), Zero())), Zero())

  // @(0|0)!(0|0)
  val reducible_4 = Output(Quote(Par(Zero(), Zero())), Par(Zero(), Zero())) // counter example

  // @0!(*@(0|0))
  val reducible_5 = Output(Quote(Zero()), Drop(Quote(Par(Zero(), Zero()))))

  // @0!(*@(0|0)) | for(@(0|0|0) <- @0){ *@(0|0|0)!( }
  val reducible_6 =
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


  // new x in { x!(0) }
  val reducible_7 =
    New(
      Var("x"),
      Output(
        Var("x"),
        Zero()
      )
    )


  // new x in { x!(0) | for(z <- x){*z} }
  val reducible_8 =
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
        )
      )
    )


  // new x in { x!(0) | for(z <- x){ *z } }
  val reducible_9 =
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


  // new x in { new y in { x!(*y) | for(u <- y){ u!0 }} | for( z <- x ){ z!(0) } }
  val reducible_10 =
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


  // new x in { @(0|0)!*x | for(z <- @0){ new y in { z!(y!(0)) | for(v <- y){ *v } | for(q <- z){ *q }}}}

  val reducible_11 =
    New(
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


  def evaluate(proc: Proc[Channel]): Unit = {

    val result = RhoInterface.reduce.run(MachineState(HashMap.empty[Channel, ChannelQueue], List(proc))).run

    for { index <- result.indices }{
      println("\n" + result(index)._1.mkString("Trace " + (index.toInt+1).toString + "\n" + "\n","\n", "\n" + "Terminated" + "\n" ))
    }
  }

  evaluate(reducible_9)
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

  case object EmptyQueue extends ChannelQueue {
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


object RhoInterface {

  def getStore: Trace[MachineState,List[MachineState],Store] = {
    Trace.get[MachineState, List[MachineState]].map { state => state.store }
  }

  def putStore: Store => Trace[MachineState, List[MachineState], Unit] =
    store =>
      Trace.modify[MachineState, List[MachineState]] {
        case MachineState(_, runQueue) => MachineState(store, runQueue)
      }

  def getRunQueue: Trace[MachineState,List[MachineState],RunQueue] = {
    Trace.get[MachineState, List[MachineState]].map { st => st.runQueue }
  }

  def putRunQueue: RunQueue => Trace[MachineState, List[MachineState], Unit] =
    runQueue =>
      Trace.modify[MachineState, List[MachineState]] {
        case MachineState(store,_) => MachineState(store,runQueue)
      }

  // cancel(@*N) = N
  def cancel: Channel => Channel = {
    case Quote(Drop(n)) => n
    case chan => chan
  }

  // Variable binding is represented by substitution in the body of the process.
  // A more efficient implementation will achieve the same result using environments.
  def bind: Channel => Channel => Proc[Channel] => Proc[Channel] =
    chan0 =>
      chan1 =>
        proc =>
          Proc.functorProc.map[Channel,Channel](proc) { name =>
            //@*@Q = @Q in P{@*@Q/z}
            if (name.equals(chan0)) cancel(chan1)
            else name
          }


  def write: Channel => ChannelQueue => Trace[MachineState, List[MachineState], Unit] =
    chan =>
      chanQ =>
        for {store <- getStore
             _ <- putStore(store + {
               chan -> chanQ
             })
        } yield ()


  def alloc: Channel => Trace[MachineState, List[MachineState], ChannelQueue] = {
    chan =>
      val e = EmptyQueue
      for {_ <- write(chan)(e)} yield e
  }


  def read: Channel => Trace[MachineState, List[MachineState], ChannelQueue] =
    chan =>
      for {store <- getStore
           chanQ1 <- store.get(chan) match {
             //I'd like to just be able to say pure(chanQ0)
             case Some(chanQ0) => Trace.pure[MachineState,List[MachineState],ChannelQueue](chanQ0)
             case None => alloc(chan)
           }
      } yield chanQ1


  // Smart constructor for readerQueue
  def readerQueue: List[Reader] => ChannelQueue = {
    case Nil => EmptyQueue
    case reader :: rs => ReaderQueue(reader, rs)
  }

  // Smart constructor for writerQueue
  def writerQueue: List[Writer] => ChannelQueue = {
    case Nil => EmptyQueue
    case writer :: ws => WriterQueue(writer, ws)
  }


  def reduce: Trace[MachineState, List[MachineState], Unit] = {

    // Get run-queue and pull first process off.
    for {runQueue <- getRunQueue

         _ <- runQueue match {

           // If the queue is empty, log the final state, and terminate.
           case Nil =>

             for {st <- Trace.get[MachineState,List[MachineState]]

                  _ <- Trace.tell(List(st))} yield ()

           case proc :: xs =>

             for {_ <- proc match {

                 // (Store, 0 :: R) -> (Store, R)
               case Zero() =>

                 for {st <- Trace.get[MachineState,List[MachineState]]

                      _ <- Trace.tell(List(st))

                      _ <- putRunQueue(xs)

                 } yield ()


               case par@Par(_*) =>

                 // Encodes non-determinism by generating an auxiliary run-queue for every permutation of the set (P1 | ... | Pn)
                 for {interleaving <- Trace.fromList[MachineState,List[MachineState],Seq[Proc[Channel]]](par.processes.permutations.toList)

                      // Adds a permutation to the original run-queue
                      newRunQueue = (interleaving ++ xs).toList

                      // Continues evaluating with new run-queue
                      _ <- putRunQueue(newRunQueue)

                 } yield ()

               case Input(z, x, k) =>

                 val abs = Abstraction(z, k)

                 for {st <- Trace.get[MachineState,List[MachineState]]

                      _ <- Trace.tell(List(st))

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
                        case EmptyQueue =>

                          for {_ <- write(x)(readerQueue(List(abs)))

                               newStore <- getStore

                               _ <- Trace.set[MachineState,List[MachineState]](MachineState(newStore, xs))

                          } yield ()
                      }
                 } yield ()


               case Output(x, q) =>

                 val atQ = Quote(q)

                 for {st <- Trace.get[MachineState,List[MachineState]]
                      _ <- Trace.tell(List(st))

                      chanQ <- read(x)

                      _ <- chanQ match {

                          // Similar to ReaderQueue rule in Input.
                        case WriterQueue(writer: Concretion, writers) =>

                          for {_ <- write(x)(writerQueue((writer :: writers) :+ Concretion(atQ)))

                               newStore <- getStore

                               _ <- Trace.set[MachineState,List[MachineState]](MachineState(newStore, xs))

                          } yield ()

                          // If reader in the queue, pull it off, bind message, and add continuation to the end of the run-queue
                        case ReaderQueue(reader, readers) =>

                          reader match {

                            case Abstraction(z, k) =>

                              for {_ <- write(x)(readerQueue(readers))

                                   newStore <- getStore

                                   _ <- Trace.set[MachineState,List[MachineState]](MachineState(newStore, xs :+ bind(z)(atQ)(k)))

                              } yield ()
                          }

                          // Similar to EmptyQueue rule in Input
                        case EmptyQueue =>

                          for {_ <- write(x)(writerQueue(List(Concretion(atQ))))

                               newStore <- getStore

                               _ <- Trace.set[MachineState,List[MachineState]](MachineState(newStore, xs))

                          } yield ()
                      }
                 } yield ()

                //
               case Drop(x) =>

                 x match {

                   case Quote(p) =>

                     for {st <- Trace.get[MachineState,List[MachineState]]

                          _ <- Trace.tell(List(st))

                          //(Store, *@Q :: R) -> (Store, Q :: R)
                          _ <- putRunQueue(p :: xs)} yield ()

                   case v@Var(_) => sys.error(s"Variable $v cannot be de-referenced")
                 }


               case New(x, k) =>

                 for {st <- Trace.get[MachineState,List[MachineState]]

                      _ <- Trace.tell(List(st))

                      _ <- alloc(x)

                      _ <- putRunQueue(k :: xs)

                 } yield ()

               case _ => sys.error("Undefined term")
             }

                  _ <- reduce

             } yield ()
         }
    } yield ()
  }
}
