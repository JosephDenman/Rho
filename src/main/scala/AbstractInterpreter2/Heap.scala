package AbstractInterpreter2

import ADT._
import AbstractInterpreter2.State.{RunQueue, Store}

import cats._

import scala.collection.immutable.HashMap

package object State {

  type Store = HashMap[Channel,ChannelQueue]

  type RunQueue = List[Proc[Channel]]

}

object Example extends App {

  // @0!0
  val reducible_1 = List(Output(Quote(Zero()),Par(Zero(),Zero())))

  // @0!(0|0)
  val reducible_2 = List(Output(Quote(Zero()),Par(Zero(),Zero())))

  // @(0|0)!0
  val reducible_3 = List(Output(Quote(Par(Zero(),Zero())),Zero()))

  // @(0|0)!(0|0)
  val reducible_4 = List(Output(Quote(Par(Zero(),Zero())),Par(Zero(),Zero()))) // counter example

  // @0!(*@(0|0))
  val reducible_5 = List(Output(Quote(Zero()),Drop(Quote(Par(Zero(),Zero())))))

  // @0!(*@(0|0)) | for(@(0|0|0) <- @0){ *@(0|0|0)!( }
  val reducible_6 = List(
    Par(
      Output(
        Quote(Zero()),
        Drop(Quote(Par(Zero(),Zero())))
      ),
      Input(
        Quote(Par(Zero(),Zero(),Zero())),
        Quote(Zero()),
        Drop(Quote(Par(Zero(),Zero(),Zero())))
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
            Output(Var("x"),Drop(Var("y"))),
            Input(Var("u"),Var("y"),Output(Var("u"),Zero()))
          )
        ),
        Input(Var("z"),Var("x"),Output(Var("z"),Zero()))
      )
    )
  )

  val store = HashMap.empty[Channel,ChannelQueue]

  for { result <- Reduce.reduce(store,reducible_10) } yield {
    println("")
    println(result._1.mkString(" , ").mkString("Final Store : { ",""," }"))
  }
}


sealed trait Channel

  //*@P = P ==> Drop(Quote(P)) = P
  //@*N = N ==> Quote(Drop(N)) = N

  case class Quote(unquote: Proc[Channel]) extends Channel {
    override def toString: String = "@(" + unquote + ")"
  }

  case class Var(id: String) extends Channel {
    override def toString: String = id
  }


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


sealed trait Reader

  case class Abstraction(z: Channel, k: Proc[Channel]) extends Reader {
    override def toString: String = " λ" + z + " { " + k.toString + " } "
  }


sealed trait Writer

  case class Concretion(q: Channel) extends Writer{
    override def toString: String =  " !" + q.toString + " "
  }


case class MachineState(store: Store, runQueue: RunQueue)

case class ReduceM[A](reduce: MachineState => List[(A, MachineState, List[MachineState])]) {

  def map[B](f: A => B): ReduceM[B] = ReduceM {
    st0 =>
      for { (ret, st1, log) <- reduce(st0) } yield {
        (f(ret), st1, log)
      }
  }

  def flatMap[B](f: A => ReduceM[B]): ReduceM[B] = ReduceM {
    st0 =>
      for { (a, st1, log0) <- reduce(st0)
            (b, st2, log1) <- f(a).reduce(st1) } yield {
        (b, st2, log0 ++ log1)
      }
  }

  def withFilter(pred: A => Boolean) = ReduceM {
    st => for (triple <- this.reduce(st); if pred(triple._1)) yield {triple}
  }

  def listen: ReduceM[(A,List[MachineState])] = ReduceM(
    st0 => for ((ret,st1,log) <- this.reduce(st0)) yield {((ret,log),st1,log)}
    )
}

object ReduceM {

  def fromList[Return]: List[Return] => ReduceM[Return] =
    rets => ReduceM(st => rets.map(ret => (ret,st,List())))

  def state[Return]: (MachineState => (Return, MachineState)) => ReduceM[Return] =
    f => ReduceM(st0 => f(st0) match {case (ret, st1) => List((ret,st1,Nil))})

  val getState: ReduceM[MachineState] = state(st => (st,st))

  val putState: MachineState => ReduceM[Unit] = st => state(st0 => ((),st))

  val getStore: ReduceM[Store] = state(st => (st.store,st))

  val putStore: Store => ReduceM[Unit] =
    str => state(st0 =>((),MachineState(str,st0.runQueue)))

  val getRunQueue: ReduceM[RunQueue] = state(st => (st.runQueue,st))

  val putRunQueue: RunQueue => ReduceM[Unit] =
    rq => state(st0 =>((),MachineState(st0.store,rq)))

  def writer[Return]: (Return, List[MachineState]) => ReduceM[Return] =
    (ret,log) => ReduceM[Return](st0 => List((ret,st0,log)))

  val tell: List[MachineState] => ReduceM[Unit] = log => writer((),log)

  implicit val reduceInstances: Monad[ReduceM] = new Monad[ReduceM]{

    def pure[Return](ret: Return) = ReduceM[Return](
      st => List((ret,st,Nil))
    )

    def flatMap[A,B](ma: ReduceM[A])(f: A => ReduceM[B]) = ma.flatMap(f)

    def tailRecM[A,B](a: A)(f: A => ReduceM[Either[A,B]]): ReduceM[B] = ???
  }
}

trait Reduce {

  val reduce: (Store,RunQueue) => List[(Store,RunQueue)]

}

object Reduce {

  val bind: Channel => Channel => Proc[Channel] => Proc[Channel] =
    z =>
      x=>
        proc =>
          Proc.functorProc.map(proc){ name =>
            if (name == z) x
            else name
          }

  val readerQueue: List[Reader] => ChannelQueue = {
    case Nil => EmptyQueue()
    case reader :: rs => ReaderQueue(reader,rs)
  }

  val writerQueue: List[Writer] => ChannelQueue = {
    case Nil => EmptyQueue()
    case writer :: ws => WriterQueue(writer,ws)
  }

  val reduceM: ReduceM[Unit] = {
    for (
      st @ MachineState(store, runQueue) <- ReduceM.getState;
      _ <- ReduceM.tell(List(st));
      _ <- runQueue match {
        case Nil => ReduceM.reduceInstances.pure(())
        case proc :: procs => {
          proc match {
            case par @ Par(_*) => {
              for (
                interleaving <- ReduceM.fromList(
                  par.processes.permutations.toList
                  );
                newRunQueue = (interleaving ++ procs).toList;
                _ <- ReduceM.putRunQueue(newRunQueue);
                _ <- reduceM
              ) yield {()}
            }
            case _ => ???
          }
        }
      }
      ) yield {()}
  }

  //     runQueue match {

  //       case Nil =>
  //         ReduceM.tell 

  //         println("P : {  }")

  //         println("Store : " + store.mkString("{ "," , "," }"))

  //         println("Queue : " + runQueue.mkString("{ "," :: "," }"))

  //         println("")

  //         println("Terminated")

  //         List {(store,runQueue)}  // Terminate
  //   }
  // )

  val reduce: (Store,RunQueue) => List[(Store,RunQueue)] = {

    (store, runQueue) =>

      println("")

      runQueue match {

        case Nil =>

          println("P : {  }")

          println("Store : " + store.mkString("{ "," , "," }"))

          println("Queue : " + runQueue.mkString("{ "," :: "," }"))

          println("")

          println("Terminated")

          List {(store,runQueue)}  // Terminate

        case proc :: xs =>

          println("P : { " + proc.toString + " }")

          println("Store : " + store.mkString("{ "," , "," }"))

          println("Queue : " + xs.mkString("{ "," :: "," }"))

          proc match {

            case Zero() =>  // Nil

              reduce(store, xs)


            case par @ Par(_*) =>  // Prl

              for { leavings <- par.processes.permutations.toList

                    newRunQ = (leavings ++ xs).toList

                    newState <- reduce(store, newRunQ)

              } yield { newState }

            case in @ Input(z, x, k) =>

              val abs = Abstraction(z, k)

              x match {

                case Quote(Drop(n)) =>

                  store.get(n) match {

                    case Some(rho) =>

                      rho match {

                        case WriterQueue(writer: Concretion, writers) =>

                          reduce(
                            store + {n -> writerQueue(writers)},
                            bind(z)(writer.q)(k) :: xs
                          )

                        case ReaderQueue(reader, readers) =>

                          reduce(
                            store + {n -> readerQueue((reader :: readers) :+ abs)},
                            xs
                          )

                        case EmptyQueue() =>

                          reduce(
                            store + {n -> readerQueue(List(abs))},
                            xs
                          )
                      }

                    case None =>

                      reduce(
                        store + {n -> EmptyQueue()},
                        in :: xs
                      )
                  }

                case _ =>

                  store.get(x) match {

                    case Some(rho) =>

                      rho match {

                        case WriterQueue(writer: Concretion, writers) =>

                          reduce(
                            store + {x -> writerQueue(writers)},
                            bind(z)(writer.q)(k) :: xs
                          )

                        case ReaderQueue(reader, readers) =>

                          reduce(
                            store + {x -> readerQueue((reader :: readers) :+ abs)},
                            xs
                          )

                        case EmptyQueue() =>

                          reduce(
                            store + {x -> readerQueue(List(abs))},
                            xs
                          )
                      }

                    case None =>

                      reduce(
                        store + {x -> EmptyQueue()},
                        in :: xs
                      )
                  }

              }


            case out @ Output(x, q) =>

              val atQ = Quote(q)

              x match {

                case Quote(Drop(n)) =>

                  store.get(n) match {

                    case Some(rho) =>

                      rho match {

                        case WriterQueue(writer: Concretion, writers) =>

                          reduce(
                            store + {
                              n -> writerQueue((writer :: writers) :+ Concretion(atQ))
                            }
                              + {
                              atQ -> EmptyQueue()
                            },
                            xs
                          )

                        case ReaderQueue(reader, readers) =>

                          reader match {

                            case Abstraction(z, k) =>

                              reduce(
                                store + {
                                  n -> readerQueue(readers)
                                }
                                  + {
                                  atQ -> EmptyQueue()
                                },
                                xs :+ bind(z)(atQ)(k)
                              )

                            case other => sys.error(s"Unrecognized input statement: $other")
                          }

                        case EmptyQueue() =>

                          reduce(
                            store + {
                              n -> writerQueue(List(Concretion(atQ)))
                            }
                              + {
                              atQ -> EmptyQueue()
                            },
                            xs
                          )
                      }

                    case None =>

                      reduce(
                        store + {
                          x -> EmptyQueue()
                        },
                        out :: xs
                      )
                  }

                case _ =>

                  store.get(x) match {

                    case Some(rho) =>

                      rho match {

                        case WriterQueue(writer: Concretion, writers) =>

                          reduce(
                            store + {
                              x -> writerQueue((writer :: writers) :+ Concretion(atQ))
                            }
                              + {
                              atQ -> EmptyQueue()
                            },
                            xs
                          )

                        case ReaderQueue(reader, readers) =>

                          reader match {

                            case Abstraction(z, k) =>

                              reduce(
                                store + {
                                  x -> readerQueue(readers)
                                }
                                  + {
                                  atQ -> EmptyQueue()
                                },
                                xs :+ bind(z)(atQ)(k)
                              )

                            case other => sys.error(s"Unrecognized input statement: $other")
                          }

                        case EmptyQueue() =>

                          reduce(
                            store + {
                              x -> writerQueue(List(Concretion(atQ)))
                            }
                              + {
                              atQ -> EmptyQueue()
                            },
                            xs
                          )
                      }

                    case None =>

                      reduce(
                        store + {
                          x -> EmptyQueue()
                        },
                        out :: xs
                      )
                  }
              }

            case Drop(x) =>

              x match {

                case Quote(p) =>

                  reduce (store, p :: xs)

                case v @ Var(id) => sys.error(s"Variable $v cannot be de-referenced")
              }


            case New(x,k) =>

              reduce(
                store + {x -> EmptyQueue()},
                k :: xs
              )
          }
      }
  }
}




/*
   State := Store x Queue, state is a store/run-queue pair

   Store : N -> O , a store is a finite partial mapping from names to actions available to be performed on those names

   Queue := { P1, P2, ..., PN }, the run-queue is a (finite?) set of processes ready to be executed

   O := { Ab1, Ab2, ..., AbN }, a channel queue may be a set of abstractions
    | { Con1, Con2, ..., ConN }, or a set of concretions

   N := An infinite set of identifiers

   @ : P -> N , quoting converts a process into a name

   * : N -> P , unquoting converts a name back into it's original process

   P,Q := 0
         | x!Q
         | for(x <- x)P
         | P|Q
         | *x

*/




