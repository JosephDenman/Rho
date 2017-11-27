
import cats.Eq

import scala.collection.immutable.Map


def ==>[A,B](a:A, b:B):(A,B) = {(a,b)}

trait Ord[K] extends Ordered[K] {
  def /[V](f: Map[K,V], l: List[(K,V)]):Map[K,V] = {
    l match {
      case Nil => f
      case (x,y):: tail => /[V](f,tail) + ( x -> y )
    }
  }
}



def bigJoin[K,V](f: Map[K,Set[V]], list: List[(K,Set[V])]):
  Map[K,Set[V]] = list match {
    case Nil => f
    case (k, v) :: tl =>
      val newVal = f.getOrElse(k, Set()) ++ v
      (f - k) + (k -> newVal)
}


def disjointUnion[K,V](o: Ord[K], l: Lattice[V]):

trait Lattice[A]{
  val bot: A
  val top: A
  def rel(x: A, y: A): Boolean
  def join(x: A, y: A): A
  def meet(x: A, y: A): A
}

object LatticeInstances {

  implicit def unitLattice: Lattice[Unit] = {

    new Lattice[Unit] {

      val bot: Unit = Unit

      val top: Unit = Unit

      def rel(x: Unit, y: Unit): Boolean = true

      def join(x: Unit, y: Unit): Unit = top

      def meet(x: Unit, y: Unit): Unit = bot
    }
  }

  implicit def productLattice[A,B](l:(Lattice[A], Lattice[B])): Lattice[(A,B)] = {

    new Lattice[(A, B)] {

      val bot = (bot,bot)

      val top = (top,top)

      def rel(a: (A, B), b: (A, B)): Boolean =
        rel(a._1: A ,b._1: A) && rel(a._2,b._2)

      def join(a: (A, B), b: (A, B)): (A, B) =
        (join(a._1: A, b._1: A), join(a._2: B,b._2: B))

      def meet(a: (A, B), b: (A, B)): (A, B) =
        (meet(a._1: A, b._1: A), meet(a._2, b._2))
    }
  }

  implicit def eqLattice[S](o: Ord[S], e: Eq[S]): Lattice[Set[S]] = {

    new Lattice[Set[S]] {

      val bot = Set.empty

      val top = "No representation of universal set"

      def rel(x: Set[S], y: Set[S]): Boolean = x subsetOf y

      def join(x: Set[S], y: Set[S]): Set[S] = x union y

      def meet(x: Set[S], y: Set[S]): Set[S] = x intersect y
    }
  }

  implicit def latticeLattice[K,V](o: Ord[K], l: Lattice[V]):Lattice[Map[K,V]] = {

    new Lattice[Map[K,V]]{

      val bot = Map.empty

      val top = "No representation of top map"

      def rel()

      def meet()

      def join()
    }
  }
}