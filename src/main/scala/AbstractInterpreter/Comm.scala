package ADT

import AbstractInterpreter.Aliases.{Channel, Env}
import AbstractInterpreter.PureStoreLike
import cats.data.ReaderT
import monix.eval.{MVar, Task}

case class Comm[A](run: ReaderT[Task,Map[String,MVar[Rho]],A])

case class _Input[Rho](x: Channel[Rho], z: Rho, k: Proc[Rho], env: Env[Channel[Rho]])

object Evaluator {

  val consumer: _Input[Rho] => Task[Proc[String]] = {
    proc =>
      // alloc for z
      // read from x
      // bind that to z
      // update environment for k


      // need to implement mutable references???
      for { addr <- PureStoreLike.mvarStore.alloc(proc.z)
            actual <- PureStoreLike.mvarStore.read(proc.x)
            rho <- PureStoreLike.mvarStore.bind(addr)(actual)} yield {}
  }
}