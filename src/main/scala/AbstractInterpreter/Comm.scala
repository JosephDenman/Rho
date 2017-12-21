package ADT

import cats.data.ReaderT
import monix.eval.{MVar, Task}

case class Comm[A](run: ReaderT[Task,Map[String,MVar[Rho]],A])