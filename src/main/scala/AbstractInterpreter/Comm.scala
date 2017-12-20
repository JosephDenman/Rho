package ADT

import cats.data._
import monix.eval._

case class Comm[A](run: ReaderT[Task,Map[String,MVar[Rho]],A])
