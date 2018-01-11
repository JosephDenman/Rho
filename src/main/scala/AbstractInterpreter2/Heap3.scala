package AbstractInterpreter2

import ADT._
import AbstractInterpreter2.State.{RunQueue, Store}

import scala.collection.immutable.HashMap

package object State {

  type Store = HashMap[Quote,ChannelQueue]

  // Store : N -> Queue

  // Drop(x) = P

  type RunQueue = List[Proc[Quote]]

}

object Example extends App {

  // @0!0

  val reducible_1 = List(
    Output(Quote(Zero()),Par(Zero(),Zero()))
  )

  // @0!(0|0)

  val reducible_2 = List(
    Output(Quote(Zero()),Par(Zero(),Zero()))
  )

  // @(0|0)!0

  val reducible_3 = List(
    Output(Quote(Par(Zero(),Zero())),Zero())
  )

  // @(0|0)!(0|0)

  val reducible_4 = List(
    Output(Quote(Par(Zero(),Zero())),Par(Zero(),Zero()))
  )

  // @0!(*@(0|0))

  val reducible_5 = List(
    Output(Quote(Zero()),Drop(Quote(Par(Zero(),Zero()))))
  )

  // @0!(*@(0|0)) | for(@(0|0|0) <- @0){ *@(0|0|0) }

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

  val store = HashMap.empty[Quote,ChannelQueue]

  for { result <- Reduce.reduce(store,reducible_6) } yield {
    println("")
    println(result._1.mkString(" , ").mkString("Final Store : { ",""," }"))
  }
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

  case class Abstraction(z: Quote, k: Proc[Quote]) extends Reader {
    override def toString: String = " λ" + z + " { " + k.toString + " } "
  }


sealed trait Writer

  case class Concretion(q: Quote) extends Writer{
    override def toString: String =  " !" + " { " + q.toString + " } "
  }


trait Reduce {

  val reduce: (Store,RunQueue) => List[(Store,RunQueue)]

}

object Reduce {

  val bind: Quote => Quote => Proc[Quote] => Proc[Quote] =
    z =>
      x=>
        proc =>
          Proc.functorProc.map(proc) { name =>
            if (name.equals(z)) x
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

  val reduce: (Store,RunQueue) => List[(Store,RunQueue)] = {

    (store, runQueue) =>

      println("")

      runQueue match {

        case Nil =>

          println("P : {  }")

          println("Store : " + store.mkString("{ "," , "," }"))

          println("RunQueue : " + runQueue.mkString("{ "," :: "," }"))

          println("") ; println("Terminated") ; List {(store,runQueue)}  // Terminate

        case proc :: xs =>

          println("P : { " + proc.toString + " }")

          println("Store : " + store.mkString("{ "," , "," }"))

          println("RunQueue : " + xs.mkString("{ "," :: "," }"))

          proc match {

            case z @ Zero() =>  // Nil

              reduce(store, xs)


            case par @ Par(_*) =>  // Prl

              for { leavings <- par.processes.permutations.toList

                    newRunQ = (leavings ++ xs).toList

                    newst <- reduce(store, newRunQ)

              } yield { newst }


            case in @ Input(z, x, k) =>

              val abs = Abstraction(z, k)

              store.get(x) match {

                case Some(rho) =>

                  rho match {

                    case ws@WriterQueue(writer: Concretion, writers) =>

                      reduce(
                        store + {
                          x -> writerQueue(writers)
                        },
                        bind(z)(writer.q)(k) :: xs
                      )

                    case rs@ReaderQueue(reader, readers) =>

                      reduce(
                        store + {
                          x -> ReaderQueue(reader, readers :+ abs)
                        },
                        xs
                      )

                    case EmptyQueue() =>

                      reduce(
                        store + {
                          x -> ReaderQueue(abs, List.empty)
                        },
                        xs
                      )
                  }

                case None =>

                  reduce(
                    store + { x -> EmptyQueue() },
                    in :: xs
                  )
              }


            case out @ Output(x, q) =>

              val atq = Quote(q)

              store.get(x) match {

                case Some(rho) =>

                  rho match {

                    case WriterQueue(writer: Concretion, writers) =>

                      reduce(
                        store + {
                          x -> writerQueue((writer :: writers) :+ Concretion(atq))
                        }
                          + {
                          atq -> EmptyQueue()
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
                              atq -> EmptyQueue()
                            },
                            xs :+ bind(z)(atq)(k)
                          )

                        case other => sys.error(s"Unrecognized input statement: $other")
                      }

                    case EmptyQueue() =>

                      reduce(
                        store + {
                          x -> writerQueue(Concretion(atq) :: List.empty)
                        }
                          + {
                          atq -> EmptyQueue()
                        },
                        xs
                      )

                  }

                case None =>

                  reduce(
                    store + { x -> EmptyQueue() },
                    out :: xs
                  )
              }

            case drop @ Drop(x) =>
              reduce(
                store,
                x.unquote :: xs
              )
          }
      }
  }
}

