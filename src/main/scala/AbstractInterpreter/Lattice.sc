package AbstractInterpreter
import cats.Eq
import cats.Order
import cats.Show
import scala.collection.immutable.Map

  sealed trait Lattice[A] {
    val bot: A
    val top: A
    val rel: (A, A) => Boolean
    val join: (A, A) => A
    val meet: (A, A) => A
  }

  object Lattice {

    implicit def unitLattice: Lattice[Unit] = {

      new Lattice[Unit] {
        val bot: Unit = Unit
        val top: Unit = Unit
        val rel: (Unit, Unit) => Boolean = (a: Unit, b: Unit) => true
        val join: (Unit, Unit) => Unit = (a: Unit, b: Unit) => top
        val meet: (Unit, Unit) => Unit = (a: Unit, b: Unit) => bot
      }
    }

    // or is it just (Lattice[A], Lattice[B]) => Lattice[(A,B)]

    implicit def productLattice[A, B](implicit l1: Lattice[A], l2: Lattice[B]): Lattice[(A, B)] = {

      new Lattice[(A, B)] {
        val bot: (Any, Any) = (bot, bot)
        val top: (Any, Any) = (top, top)
        val rel: ((A, B), (A, B)) => Boolean = (a: (A, B), b: (A, B)) => l1.rel(a._1, a._2) && l2.rel(b._1, b._2)
        val join: ((A, B), (A, B)) => (A, B) = (a: (A, B), b: (A, B)) => (l1.join(a._1, a._2), l2.join(b._1, b._2))
        val meet: ((A, B), (A, B)) => (A, B) = (a: (A, B), b: (A, B)) => (l1.meet(a._1, a._2), l2.meet(b._1, b._2))
      }
    }

    implicit def ordEqLattice[S](implicit o: Order[S], e: Eq[S]): Lattice[Set[S]] = {

      new Lattice[Set[S]] {
        val bot: Set[S] = Set.empty[S]
        val top: Set[S] = System.out.println("error")
        val rel: (Set[S], Set[S]) => Boolean = (a: Set[S], b: Set[S]) => a subsetOf b
        val join: (Set[S], Set[S]) => Set[S] = (a: Set[S], b: Set[S]) => a union b
        val meet: (Set[S], Set[S]) => Set[S] = (a: Set[S], b: Set[S]) => a intersect b
      }
    }

    implicit def mapLattice[K, V](implicit o: Order[K], l: Lattice[V]): Lattice[Map[K, V]] = {

      new Lattice[Map[K, V]] {
        val bot: Map[K, V] = Map.empty
        val top: Map[K, V] = System.out.println("error")
        val rel: (Map[K, V], Map[K, V]) => Boolean = (f: Map[K, V], g: Map[K, V]) => f.isSubmapOfBy(g)(l.rel)
        val join: (Map[K, V], Map[K, V]) => Map[K, V] = (f: Map[K, V], g: Map[K, V]) => f.unionWith(g)(l.join)
        val meet: (Map[K, V], Map[K, V]) => Map[K, V] = (f: Map[K, V], g: Map[K, V]) => f.intersectionWith(g)(l.meet)
      }
    }


    implicit def ⊎[K,V](m: Map[K,V], xs: List[(K,V)])(implicit o: Order[K], l: Lattice[V])

    implicit def ⨆[K, V](m: Map[K, V], xs: List[(K, V)])(implicit o: Order[K], l: Lattice[V]): Map[K, V]

    implicit def !![K, V](m: Map[K, V])(implicit o: Order[K], l: Lattice[V]): K => V




    implicit class MapTheory[K, A](private val m1: Map[K, A]) {

      def insertWith[A1 >: A](k: K, v: A1, f: (A1, A1) => A1): Map[K, A1] = {
        m1.updated(k, m1.get(k).map(f(v, _)).getOrElse(v))
      }

      def unionWithKey(m2: Map[K, A])(f: (K, A, A) => A): Map[K, A] = {
        val diff = m2 -- m1.keySet
        val aug = m1 map {
          case (k, v) => if (m2 contains k) k -> f(k, v, m2(k)) else (k, v)
        }
        aug ++ diff
      }

      def unionWith(m2: Map[K, A])(f: (A, A) => A): Map[K, A] = {
        unionWithKey(m2)((_, x, y) => f(x, y))
      }


      def intersectionWithKey[B, C](m2: Map[K, B])(f: (K, A, B) => C): Map[K, C] = {
        m1 collect {
          case (k, v) if m2 contains k => k -> f(k, v, m2(k))
        }
      }

      def intersectionWith[B, C](m2: Map[K, B])(f: (A, B) => C): Map[K, C] = {
        intersectionWithKey(m2)((_, x, y) => f(x, y))
      }

      // True to haskell definition? //

      def isSubmapOfBy[B](m2: Map[K, B])(g: (A, B) => Boolean): Boolean = {
        val f: (K, A) => Boolean = (k: K, v: A) => (m2 contains k) && g(v, m2(k))
        m1 forall { case (k, v) => f(k, v) }
      }

      def bigJoin[V](f: Map[K, Set[V]], list: List[(K, Set[V])]): Map[K, Set[V]] = list match {
        case Nil => f
        case (k, v) :: tl =>
          val newVal = f.getOrElse(k, Set()) ++ v
          (f - k) + (k -> newVal)
      }
    }
  }




