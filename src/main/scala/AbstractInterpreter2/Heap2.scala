package AbstractInterpreter2

// Names
// @0 = Quote(Zero())
// @(0|0) = Quote(Par(Zero(),Zero())
// @(0!(0))

// Processes
// @0 ! (0)
// @(0|0) ! (0)
// @(0!(0)) ! (0)

// Output(Quote(Zero()),Drop("x")) = @0!(*x)
// Chan

// Output("@0",Drop("x")) = @0!(*x)
// Var

// new @0 in { P }

// @ : Clo -> Channel

// @ : Proc -> Var

/*
package object State {

  type Environment = HashMap[Rho,Channel]

  type Store = HashMap[Channel,ChannelQueue]

  type RunQueue = List[Clo]

}

// @(*x) = x

// @(for(@0 <- @0)P)

// @0!(*x)

// Output(Right(Rho(Zero())),Drop(Left("x")))

// Drop(Right(Rho(P))) => P == *(@0) = 0

// Rho(Drop(Rho(Zero())))) => Rho(Zero()) == @(*(@0)) = @0

// Proc[Either[Var,Rho]]

// @Q!*@S | for( @R <- @Q ){ *@R } ==> *@R{@*@S/@R} == *@*@S = S

// x!(*y) | for( z <- x ){ *z } ==> *z{@*y/z} == *y

// Env : Rho -> Rho

case class Clo(proc: Proc[Rho], env: Environment){ // rethink env in quoting
  override def toString: String = proc.toString
}

// {@Q/@R} := env + (@R -> @Q)

// Env : Rho -> Channel

// Env : Var -> Channel

sealed trait Channel

case class Quote(unquote: Clo) extends Channel {
  val hash: String = Hashids(super.hashCode().toString).encode(1L)
  override def toString: String = "@(" + unquote.toString + ")"
  override def equals(o: Any): Boolean = {
    o match {
      case that: Quote => that.hash == this.hash
      case _ => super.equals(o)
    }
  }
}


sealed trait ChannelQueue

  case class ReaderQueue(x:(Reader,Environment), xs: List[(Reader,Environment)]) extends ChannelQueue {
    override def toString: String = (x :: xs).map{_._1.toString}.mkString("[", "][", "]")
  }

  case class WriterQueue(x: Writer, xs: List[Writer]) extends ChannelQueue {
    override def toString: String = (x :: xs).map{_.toString}.mkString("[", "][", "]")
  }

  case class EmptyQueue() extends ChannelQueue {
    override def toString: String = "[]"
  }


sealed trait Reader

  case class Abstraction(z: Rho, k: Proc[Rho]) extends Reader {
    override def toString: String = " λ" + z + " { " + k.toString + " } "
  }


sealed trait Writer

  case class Concretion(q: Quote) extends Writer{
    override def toString: String =  " !" + " { " + q.toString + " } "
  }


object Example extends App {

  // @0!0


  val reducible_1 = List(
    Clo(Output(Rho(Zero()),Par(Zero(),Zero())),HashMap.empty[Rho,Channel])
  )

  // @0!(0|0)

  val reducible_2 = List(
    Clo(Output(Rho(Zero()),Par(Zero(),Zero())),HashMap.empty[Rho,Channel]
    )
  )

  // @(0|0)!0

  val reducible_3 = List(
    Clo(Output(Rho(Par(Zero(),Zero())),Zero()),HashMap.empty[Rho,Channel]
    )
  )

  // @(0|0)!(0|0)

  val reducible_4 = List(
    Clo(Output(Rho(Par(Zero(),Zero())),Par(Zero(),Zero())),HashMap.empty[Rho,Channel]
    )
  )

  // @0!(*@(0|0))

  val reducible_5 = List(
    Clo(Output(Rho(Zero()),Drop(Rho(Par(Zero(),Zero())))),HashMap.empty[Rho,Channel]
    )
  )

  // @0!(*@(0|0)) | for(@(0|0|0) <- @0){ *@(0|0|0) }
  val reducible_6 = List(
    Clo(
      Par(
        Output(
          Rho(Zero()),
          Drop(Rho(Par(Zero(),Zero())))
        ),
        Input(
          Rho(Par(Zero(),Zero(),Zero())),
          Rho(Zero()),
          Drop(Rho(Par(Zero(),Zero(),Zero())))
        ),
      ),
        HashMap.empty[Rho,Channel]
    )
  )


  for { result <- Reduce.reduce(HashMap.empty[Channel,ChannelQueue],reducible_6) } yield {
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

              env.get(x) match {

                case Some(chan) =>

                  val message = Quote(Clo(q, env))

                  store(chan) match {

                    case WriterQueue(writer: Concretion, writers) =>

                      val message = Quote(Clo(q, env))

                      reduce(
                        store + {
                          chan -> WriterQueue(
                            writer,
                            writers :+ Concretion(message)
                          )
                        } + {
                          message -> EmptyQueue()
                        },
                        xs
                      )

                    case ReaderQueue(reader: (Reader, Environment), readers) =>

                      val env = reader._2

                      val abs = reader._1

                      abs match {

                        case Abstraction(z, k) =>

                          reduce(
                            store + {
                              chan -> readerQueue(readers)
                            } + {
                              message -> EmptyQueue()
                            }, // empty channel queues are allocated whenever a process is quoted
                            xs :+ Clo(k, env + {
                              z -> message
                            })
                          )

                        case _ => sys.error("Unrecognized reader")
                      }

                    case EmptyQueue() =>
                      reduce(
                        store + {
                          chan -> WriterQueue(Concretion(message), List.empty)
                        } + {
                          message -> EmptyQueue()
                        },
                        xs
                      )
                  }


                case None =>

                  val chan = Quote(Clo(x.unquote,env))

                  reduce(
                    store + {chan -> EmptyQueue()},
                    Clo(out, env + {x -> chan}) :: xs
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

           /* case neu @ New(x,k) =>

              val chan = Quote(Clo(Zero(),HashMap.empty[Rho, Channel]))
              // a non-unique name will not update correctly
              // notice that names are not only allocated here, but during variable binding.
              reduce(
                store + {chan -> EmptyQueue()},
                Clo(k, env + {x -> chan}) :: xs
              ) */
          }
      }
  }
} */

// Output(Quote(Zero()),P|Q)

// for ( z <- x )P
// env(x) match {
    // case Some(name) => reduce( (P ,env + { z -> name }), queue)
    // case None =>

// Env : Rho -> Rho ==> {@Q/@R}

// Env { x : Rho -> Rho, z : Rho -> Rho }

// Store { Rho -> ChannelQueue, Rho -> Empty }

// Store : Channel -> Queue
// Channel := @(P x Env)

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
