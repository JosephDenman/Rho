package AbstractInterpreter

import cats._
import cats.data._
import cats.instances.list._
import cats.Monad
import cats.Monoid._

object Trace {

  type Trace[S,W,A] = StateT[WriterT[List,W,?],S,A]

  def get[S,W](implicit W: Monoid[W]): Trace[S,W,S] =
    StateT.get[WriterT[List[?],W,?],S](WriterT.catsDataApplicativeForWriterT(catsStdInstancesForList,W))
  
  def set[S,W](s: S)(implicit W: Monoid[W]): Trace[S,W,Unit] =
    StateT.set[WriterT[List[?],W,?],S](s)(WriterT.catsDataApplicativeForWriterT(catsStdInstancesForList,W))

  def modify[S,W](f: (S) => S)(implicit W: Monoid[W]): Trace[S,W,Unit] =
    StateT.modify[WriterT[List[?],W,?],S](f)(WriterT.catsDataApplicativeForWriterT(catsStdInstancesForList,W))

  def tell[S,W](w: W)(implicit W: Monoid[W]): Trace[S,W,Unit] =
    StateT.lift[WriterT[List[?],W,?],S,Unit](WriterT.tell(w))(WriterT.catsDataApplicativeForWriterT(catsStdInstancesForList,W))

  def fromList[S,W,A](xs: List[A])(implicit W: Monoid[W]): Trace[S,W,A] =
    StateT.lift[WriterT[List[?],W,?],S,A](WriterT.lift(xs))(WriterT.catsDataApplicativeForWriterT(catsStdInstancesForList,W))
}
