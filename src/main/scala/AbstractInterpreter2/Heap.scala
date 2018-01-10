package AbstractInterpreter2

import ADT._
import AbstractInterpreter.StateSpace.Var
import AbstractInterpreter2.State.{Environment, RunQueue, Store}
import org.hashids.Hashids

import scala.collection.immutable.HashMap

package object State {

  type Environment = HashMap[Var,Channel]

  type Store = HashMap[Channel,ChannelQueue]

  type RunQueue = List[Clo]

}

case class Clo(proc: Proc[Var],env: Environment){
  override def toString: Var = proc.toString
}


sealed trait Channel

  case class Quote(unquote: Clo) extends Channel {
    val hash: String = Hashids(super.hashCode().toString).encode(1L)
    override def toString: Var = "@(" + unquote.toString + ")"
    override def equals(o: Any): Boolean = {
      o match {
        case that: Quote => that.hash == this.hash
        case _ => super.equals(o)
      }
    }
  }


sealed trait ChannelQueue

  case class ReaderQueue(x:(Reader,Environment), xs: List[(Reader,Environment)]) extends ChannelQueue {
    override def toString: Var = (x :: xs).map{_._1.toString}.mkString("[", "][", "]")
  }

  case class WriterQueue(x: Writer, xs: List[Writer]) extends ChannelQueue {
    override def toString: Var = (x :: xs).map{_.toString}.mkString("[", "][", "]")
  }

  case class EmptyQueue() extends ChannelQueue {
    override def toString: Var = "[]"
  }


sealed trait Reader

  case class Abstraction(z: Var, k: Proc[Var]) extends Reader {
    override def toString: String = " λ" + z + " { " + k.toString + " } "
  }


sealed trait Writer

  case class Concretion(q: Quote) extends Writer{
    override def toString: String =  q.toString + ", " + "Env " + q.unquote.env.mkString("{ "," , "," }")
  }


object Example extends App {

  // new x in {x!(0|0) | for( z <- x ){ *z }}
  val reducible_1 = List(
    Clo(
      New(
        "x",
        Par(
          Output(
            "x",
            Par(Zero(), Zero())),
          Input("z", "x", Drop("z"))
        )
      ),HashMap.empty[Var,Channel]
    )
  )

  // new x in { x!(*z) | for( y <- x ){ y!(0|0) } }
  val reducible_2 = List(
    Clo(
      New(
        "x",
        Par(
          Output("x", Drop("z")),
          Input("y","x", Output("y", Par(Zero(),Zero())))
        )
      ), HashMap.empty[Var,Channel]
    )
  )

  // new x in { x!(for(z <- x){*z}) | for( y <- x ){ *y | y!(0|0) | for(w <- y){*w}}
  val reducible_3 = List(
    Clo(
      New(
        "x",
        Par(
          Output("x",Input("z","x",Drop("z"))),
          Input("y","x",Par(Drop("y"),Output("y",Par(Zero(),Zero())),Input("w","y",Drop("w"))))
        )
      ), HashMap.empty[Var,Channel]
    )
  )

  // new x in { new y in { x!(*y) | for( z <- x ){ z!(0|0) } } }
  val reducible_4 = List(
    Clo(
      New(
        "x",
        New(
          "y",
          Par(
            Output("x",Drop("y")),
            Input("z","x",Output("z",Par(Zero(),Zero())))
          )
        )
      ),HashMap.empty[Var,Channel]
    )
  )

  // new x in { x!(0) | for(z <- x){ *z }}} | new y in { y!(0) | for(w <- y){ *w }}}
  val reducible_5 = List(
    Clo(
      Par(
        New(
          "x",
          Par(
            Output("x",Zero()),
            Input("z","x",Drop("z"))
          )
        ),
        New(
          "y",
          Par(
            Output("y",Zero()),
            Input("w","y",Drop("w"))
          )
        )
      ),HashMap.empty[Var,Channel]
    )
  )

  for { result <- Reduce.reduce(HashMap.empty[Channel,ChannelQueue],reducible_4) } yield {
    println("")
    println(result._1.mkString(" , ").mkString("Final Store : { ",""," }"))
  }
}

trait Reduce {

  val reduce: (Store,RunQueue) => List[(Store,RunQueue)]

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

  val reduce: (Store,RunQueue) => List[(Store,RunQueue)] = {

    (store, runQueue) =>

      println("")

      runQueue match {

        case Nil =>

          println("P : {  }")

          println("Store : " + store.mkString("{ "," , "," }"))

          println("RunQueue : " + runQueue.mkString("{ "," :: "," }"))

          println("") ; println("Terminated") ; List {(store,runQueue)}  // Terminate

        case Clo(proc, env) :: xs =>

          println("P : { " + proc.toString + " }")

          println("Env : " + env.mkString("{ "," , "," }"))

          println("Store : " + store.mkString("{ "," , "," }"))

          println("RunQueue : " + xs.mkString("{ "," :: "," }"))

          proc match {

            case z @ Zero() =>  // Nil

              reduce(store, xs)

            case par @ Par(_*) =>  // Prl

              for { leavings <- par.processes.permutations.toList

                    newRunQ = (leavings.map { proc => Clo(proc,env) } ++ xs).toList

                    newst <- reduce(store, newRunQ)

              } yield { newst }


            case in @ Input(z, x, k) =>

              val chan = env(x)

              store(chan) match {

                case ws @ WriterQueue(writer: Concretion, writers) =>

                  val message = writer.q

                    reduce(
                      store + { chan -> writerQueue(writers) },
                      Clo(k, env + { z -> message }) :: xs
                    )

                case rs @ ReaderQueue(reader, readers) =>

                  val abs = Abstraction(z, k)

                    reduce(
                      store + {
                        chan -> ReaderQueue(
                                  reader,
                                  readers :+ (abs, env)
                                )
                      },
                      xs
                    )

                case EmptyQueue() =>

                    reduce(
                      store + {
                        chan -> ReaderQueue(
                                  (Abstraction(z, k), env),
                                  List.empty
                                )
                      },
                      xs
                    )
              }


            case out @ Output(x, q) =>

              val chan = env(x)

              val message = Quote(Clo(q,env))

              store(chan) match {

                case WriterQueue(writer: Concretion, writers) =>

                  val message = Quote(Clo(q, env))

                    reduce(
                      store + {
                        chan -> WriterQueue(
                                  writer,
                                  writers :+ Concretion(message)
                                )
                      } + {message -> EmptyQueue()},
                      xs
                    )

                case ReaderQueue(reader: (Reader, Environment), readers) =>

                  val env = reader._2

                  val abs = reader._1

                  abs match {

                    case Abstraction(z, k) =>

                        reduce(
                          store + { chan -> readerQueue(readers) } + { message -> EmptyQueue() }, // empty channel queues are allocated whenever a process is quoted
                          xs :+ Clo(k, env + { z -> message })
                        )

                    case _ => sys.error("Unrecognized reader")
                  }

                case EmptyQueue() =>
                    reduce(
                      store + {chan -> WriterQueue(Concretion(message), List.empty)} + { message -> EmptyQueue() },
                      xs
                    )
                  }


            case drop @ Drop(x) =>

              val chan = env(x)

              chan match {

                case Quote(unquote) =>

                  reduce(
                    store,
                    /* this means that the environment displayed at the execution of unquote is the env
                     * saved at the time of sending */
                    unquote :: xs
                  )

                case _ => sys.error("Unrecognized channel type")
            }


            case neu @ New(x,k) =>

              val chan = Quote(Clo(Zero(),HashMap.empty[Var, Channel]))
              // a non-unique name will not update correctly
              // notice that names are not only allocated here, but during variable binding.
              reduce(
                store + {chan -> EmptyQueue()},
                Clo(k, env + {x -> chan}) :: xs
              )
          }
      }
  }
}

// Output(Quote(Zero()),P|Q)

// Quote

/*
Concrete State Space:

State := Store x Kont
  - states are represented as a store/run-queue pair

Env : Var -> Chan
  - a finite mapping of free variables to channels

Store : Chan -> Queue
  - a finite mapping from a channel to a set of possible actions on that channel

Chan := @ Clo

Queue := (λx.P, Env) ,..., (λx.P, Env) - queue may be a set of abstractions paired with closing environments
       | Chan ,..., Chan - or a set of quoted closures

Kont := Clo1, ..., CloN
  - at each transition, a closure is picked off the front of the run-queue and executed

Clo := P x Env
  - where Env maintains the values of the free variables in P

P,Q := 0
     | x!Q
     | for(z <- x)P
     | P|Q
     | *x

Var := An infinite set of identifiers

@ : Clo -> Chan
  - converts a closure into a channel

*/