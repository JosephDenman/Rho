package AbstractInterpreter2

import ADT._
import AbstractInterpreter.RhoInterface.debug
import AbstractInterpreter.StateSpace.Var
import AbstractInterpreter2.State.{Environment, Heap, RunQueue}
import cats._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.HashMap

package object State {

  type Environment = HashMap[Var,Channel]

  type Heap = HashMap[Channel,ChannelQueue]

  type RunQueue = List[Clo]

}

case class Clo(proc: Proc[Var],env: Environment){
  override def toString: Var = proc.toString
}


sealed trait Channel

  case class Quote(unquote: Clo) extends Channel{
    override def toString: Var = "@(" + unquote.proc + ")"
  }


sealed trait ChannelQueue

  case class ReaderQueue(x:(Reader,Environment), xs: List[(Reader,Environment)]) extends ChannelQueue{
    override def toString: Var = "[" + x + xs.mkString(",") + "]"
  }

  case class WriterQueue(x: Writer, xs: List[Writer]) extends ChannelQueue{
    override def toString: Var = "[" + x + xs.mkString(",") + "]"
  }

  case class EmptyQueue() extends ChannelQueue{
    override def toString: Var = "[]"
  }


sealed trait Reader

  case class Abstraction(z: Var, k: Proc[Var]) extends Reader {
    override def toString: String = "λ" + z + k.toString
  }


sealed trait Writer

  case class Concretion(q: Clo) extends Writer{
    override def toString: String = "!(" + q.proc.toString + ")"
  }


object Example extends App {

  val reducible_1 = List(Clo(New("x", Par(Output("x", Par(Zero(), Zero())), Input("z", "x", Drop("z")))),HashMap.empty[Var,Channel]))

  val reducible_2 = List(Clo(New("x", Par(Output("x", Drop("z")), Input("y","x", Output("y", Par(Zero(),Zero()))))), HashMap.empty[Var,Channel]))

  val reducible_3 = List(

    Clo(
      New("x",
        Par(
          Output("x",
            Par(
              Zero(),
              Zero()
            )
          ),
          Input(
            "z",
            "x",
            Drop("z")
          ),
          Output(
            "x",
            Input(
              "w",
              "x",
              Par(
                Zero(),
                Zero()
              )
            )
          )
        )
      ),HashMap.empty[Var,Channel]
    )
  )

  for { result <- Reduce.reduce(HashMap.empty[Channel,ChannelQueue],reducible_3).runAsync } yield {
    result
  }

}

trait Reduce{

  val reduce: (Heap,RunQueue) => Option[(Heap,RunQueue)]

}

object Reduce {

  val readerQueue: List[(Reader,Environment)] => ChannelQueue = {
    case Nil => EmptyQueue()
    case reader :: rs => ReaderQueue(reader,rs)
  }

  val writerQueue: List[Writer] => ChannelQueue = {
    case Nil => EmptyQueue()
    case writer :: ws => WriterQueue(writer,ws)
  }

  val reduce: (Heap,RunQueue) => Task[List[(Heap,RunQueue)]] = {

    (heap, runQueue) =>

      debug("Heap: " + heap.mkString("{ "," , "," }"))

      debug("Stack: " + runQueue.mkString("{ "," , "," }"))

      runQueue match {

        case Nil => debug("Terminated") ; Task {List((heap,runQueue))} // Terminate

        case Clo(proc, env) :: xs =>

          proc match {

            case z @ Zero() =>  // Nil

              debug(z.toString)

              reduce(heap, xs)


            case par @ Par(_*) =>  // Prl

              debug(par.toString)

              /* -- work in progress:
              Task.gather(par.processes.map { proc =>
                Task fork {
                  reduce(heap, Clo(proc,env) :: xs)
                }
              })*/

              val newRunQueues = {
                par.processes.permutations.map{
                  procs => procs.toList.map{proc => Clo(proc,env)} ++ xs
                }.toList
              }

              Task.traverse(newRunQueues){
                newRunQueue => reduce (heap, newRunQueue)
              }.map(_.flatten)

            case in @ Input(z, x, k) =>

              debug(in.toString)

              val chan = env(x)

              heap(chan) match {

                case ws@WriterQueue(writer: Concretion, writers) =>

                  val message = Quote(writer.q)

                    debug("-> " + k + "[ " + z + " := " + message + " ]")

                    reduce(
                      heap + {
                        chan -> writerQueue(writers)
                      } + {
                        message -> EmptyQueue()
                      },
                      Clo(
                        k,
                        env + {
                          z -> message
                        }
                      ) :: xs
                    )


                case rs@ReaderQueue(reader, readers) =>

                  val abs = Abstraction(z, k)

                    reduce(
                      heap + {
                        chan -> ReaderQueue(
                                  reader,
                                  readers :+ (abs, env)
                                )
                      },
                      xs
                    )


                case EmptyQueue() =>

                    reduce(
                      heap + {
                        chan -> ReaderQueue(
                                  (Abstraction(z, k), env),
                                  List.empty
                                )
                      },
                      xs
                    )
                  }


            case out @ Output(x, q) =>

              debug(out.toString)

              val chan = env(x)

              val clo = Clo(q, env)

              heap(chan) match {

                case WriterQueue(writer: Concretion, writers) =>

                    reduce(
                      heap + {
                        chan -> WriterQueue(
                                  writer,
                                  writers :+ Concretion(clo)
                                )
                      },
                      xs
                    )

                case ReaderQueue(reader: (Reader, Environment), readers) =>

                  val env = reader._2

                  val abs = reader._1

                  abs match {

                    case Abstraction(z, k) =>

                        reduce(
                          heap + { chan -> readerQueue(readers) },
                          xs :+ Clo(k, env + { z -> Quote(clo)}) // Double check reader is added to end of run queue
                        )


                    case _ => sys.error("Unrecognized reader")
                  }

                case EmptyQueue() =>
                    reduce(
                      heap + { chan -> WriterQueue(Concretion(clo), List.empty)},
                      xs
                    )
                  }


            case drop @ Drop(x) =>

              debug(drop.toString)

              val chan = env(x)

              chan match {

                case Quote(unquote) =>

                  reduce(
                    heap,
                    unquote :: xs
                  )

                case _ => sys.error("Unrecognized channel type")
            }


            case neu @ New(x,k) =>

              debug(neu.toString)

              val clo = Clo(Zero(), HashMap.empty[Var, Channel])

              val chan = Quote(clo)

              reduce(
                heap + {chan -> EmptyQueue()},
                Clo(k, env + {x -> chan}) :: xs
              )
          }
      }
  }
}