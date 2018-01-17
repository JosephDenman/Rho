package AbstractInterpreter2

import ADT._
import AbstractInterpreter2.State.{RunQueue, Store}

import scala.collection.immutable.HashMap

package object State {

  type Store = HashMap[Channel,ChannelQueue]

  type RunQueue = List[Proc[Channel]]

}

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

  for { _ <- ReduceM.putState(MachineState(HashMap.empty[Channel, ChannelQueue], List(Zero())))

        st = Reduce.reduceM

  } yield {
    println(st)
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

  def withFilter(pred: A => Boolean): ReduceM[A] = ReduceM {
    state =>
      for {triple <- reduce(state) ; if pred(triple._1)} yield triple
  }

  def listen: ReduceM[(A,List[MachineState])] = ReduceM {
    state0 =>
      for {(ret, state1, log) <- reduce(state0)} yield {
       ((ret, log), state1, log)
      }
  }
}

object ReduceM {

  val getState: ReduceM[MachineState] =
    state {
      st => (st,st)
    }

  val putState: MachineState => ReduceM[Unit] =
    state1 =>
      state {
        state0 => ((),state1)
      }

  val getStore: ReduceM[Store] =
    state {
      state => (state.store,state)
    }

  val putStore: Store => ReduceM[Unit] =
    store =>
      state {
        state0 => ((), MachineState(store, state0.runQueue))
      }

  val getRunQueue: ReduceM[RunQueue] =
    state {
      st => (st.runQueue,st)
    }

  val putRunQueue: RunQueue => ReduceM[Unit] =
    runQ =>
      state {
        state0 => ((), MachineState(state0.store,runQ))
      }

  val tell: List[MachineState] => ReduceM[Unit] =
    log =>
      writer((),log)

  def fromList[A](xs: List[A]): ReduceM[A] = ReduceM {
    state =>
      xs map { xs => (xs, state, List()) }
  }

  def state[A](f: MachineState => (A, MachineState)): ReduceM[A] = ReduceM {
    state0 =>
      f(state0) match {
        case (ret, state1) => List {(ret, state1, Nil)}
      }
  }

  def pure[A](ret: A): ReduceM[A] = ReduceM {
    st =>
      List {(ret, st, Nil)}
  }

  def flatMap[A,B](ma: ReduceM[A])(f: A => ReduceM[B]): ReduceM[B] = ma.flatMap(f)

  def writer[A](entry: (A, List[MachineState])): ReduceM[A] = ReduceM {
    state =>
      List {(entry._1, state, entry._2)}
  }

}

object Reduce {

  val bind: Channel => Channel => Proc[Channel] => Proc[Channel] =
    z =>
      x =>
        proc =>
          Proc.functorProc.map(proc) { name =>
            if (name == z) x
            else name
          }


  val write: Channel => ChannelQueue => ReduceM[Store] =
    chan =>
      chanQ =>
        for {store <- ReduceM.getStore
             _ <- ReduceM.putStore(store + {
               chan -> chanQ
             })
             store_ <- ReduceM.getStore
        } yield store_


  val alloc: Channel => ReduceM[ChannelQueue] =
    chan =>
      for {e <- ReduceM.pure(EmptyQueue())
           _ <- write(chan)(e)
      } yield e


  val read: Channel => ReduceM[ChannelQueue] =
    chan =>
      for {store <- ReduceM.getStore
           queue <- store.get(chan) match {
             case Some(rho) => ReduceM.pure(rho)
             case None => alloc(chan)
           }
      } yield queue


  val normalize: Channel => Channel = {
    case Quote(Drop(n)) => normalize(n)
    case chan => chan
  }


  val readerQueue: List[Reader] => ChannelQueue = {
    case Nil => EmptyQueue()
    case reader :: rs => ReaderQueue(reader, rs)
  }


  val writerQueue: List[Writer] => ChannelQueue = {
    case Nil => EmptyQueue()
    case writer :: ws => WriterQueue(writer, ws)
  }


  val reduceM: ReduceM[Unit] = {

    println("Started")

    for {st@MachineState(store, runQueue) <- ReduceM.getState

         _ <- ReduceM.tell(List(st))

         _ = runQueue match {

           case Nil => st

           case proc :: xs =>

             proc match {

               case zero@Zero() =>

                 for {_ <- ReduceM.putRunQueue(xs)

                      st_ <- reduceM

                 } yield ()

               case _ => sys.error("Undefined term")

             }

         }

    } yield ()
  }
}

/*
               case par@Par(_*) =>

                 for {interleaving <- ReduceM.fromList(par.processes.permutations.toList)

                      newRunQueue = (interleaving ++ xs).toList

                      _ <- ReduceM.putRunQueue(newRunQueue)

                      _ <- reduceM

                 } yield {
                   ()
                 }


               case in@Input(z, x, k) =>

                 val abs = Abstraction(z, k)

                 for {chanQ <- read(x)

                      _ <- chanQ match {

                        case WriterQueue(writer: Concretion, writers) =>

                          for {newStore <- write(x)(writerQueue(writers))

                               _ <- ReduceM.putState(MachineState(newStore, bind(z)(writer.q)(k) :: xs))

                               _ <- reduceM

                          } yield {
                            ()
                          }

                        case ReaderQueue(reader, readers) =>

                          for {newStore <- write(x)(readerQueue((reader :: readers) :+ abs))

                               _ <- ReduceM.putState(MachineState(newStore, xs))

                               _ <- reduceM

                          } yield {
                            ()
                          }

                        case EmptyQueue() =>

                          for {newStore <- write(x)(readerQueue(List(abs)))

                               _ <- ReduceM.putState(MachineState(newStore, xs))

                               _ <- reduceM

                          } yield {
                            ()
                          }
                      }

                 } yield {
                   ()
                 }

               case out@Output(x, q) =>

                 val atQ = Quote(q)

                 for {chanQ <- read(x)

                      _ <- chanQ match {

                        case WriterQueue(writer: Concretion, writers) =>

                          for {_ <- alloc(atQ) // is it necessary to allocate this channel?

                               newStore <- write(x)(writerQueue((writer :: writers) :+ Concretion(atQ)))

                               _ <- ReduceM.putState(MachineState(newStore, xs))

                               _ <- reduceM

                          } yield {
                            ()
                          }

                        case ReaderQueue(reader, readers) =>

                          reader match {

                            case Abstraction(z, k) =>

                              for {_ <- alloc(atQ)

                                   newStore <- write(x)(readerQueue(readers))

                                   _ <- ReduceM.putState(MachineState(newStore, xs :+ bind(z)(atQ)(k)))

                              } yield {
                                ()
                              }
                          }

                        case EmptyQueue() =>

                          for {_ <- alloc(atQ)

                               newStore <- write(x)(writerQueue(List(Concretion(atQ))))

                               _ <- ReduceM.putState(MachineState(newStore, xs))

                          } yield {
                            ()
                          }

                      }

                 } yield {
                   ()
                 }

               case drop@Drop(x) =>

                 x match {

                   case Quote(p) =>

                     for {_ <- ReduceM.putRunQueue(p :: xs)

                          _ <- reduceM

                     } yield {
                       ()
                     }

                   case v@Var(id) => sys.error(s"Variable $v cannot be de-referenced")
                 }

               case neu@New(x, k) =>

                 for {_ <- alloc(x)

                      _ <- ReduceM.putRunQueue(k :: xs)

                      _ <- reduceM

                 } yield {
                   ()
                 }
             }
         }

    } yield {
      ()
    }
}*/


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
/*
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




