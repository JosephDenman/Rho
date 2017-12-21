package AbstractInterpreter
import ADT.Rho
import AbstractInterpreter.Aliases.{Channel, Clo, Store}
import monix.eval.{MVar, Task}

import scala.collection.immutable.HashMap

sealed trait StoreLike[A,S,C]{

  val apply: S

  val bind: S => C => A

  val write: S => A => C => S

  val read: S => A => Option[C]

  val filter: S => (A => Boolean) => S

  val alloc: Store[Int] => Int = {
    store =>
      store.keys.foldLeft(0){(a,b) => Math.max(a,b)} + 1
  }
}

object StoreLike {

  /*implicit def abstractStore: StoreLike[Int, Store[Int], Clo[Int]] = {

    new StoreLike[Int, Store[Int], Clo[Int]]{

      val apply: Store[Int] = new HashMap[Int,Clo[Int]]

      /*
        Joining(⨆) on the abstract store allows each address
        in the finite set of abstract addresses to represent
        multiple concrete addresses....the abstract allocator
        distinguishes between bindings of the same variable in
        different contexts. It determines how many abstract
        variants are associated with a variable.
      */

      val bind: Store[Int] => Clo[Int] => Int = {
        store =>
          data =>
            val address = alloc(store)
              ⨆[Int, Clo[Int]](store, List(address -> data))
              address
      }

      val write: Store[Int] => Int => Clo[Int] => Store[Int] = {
        store =>
          address =>
            data =>
              ⨆[Int, Clo[Int]](store, List(address -> data))
      }

      val read: Store[Int] => Int => Option[Clo[Int]] = {
        store =>
          address =>
            Some(!![Int, Clo[Int]](store).apply(address))
      }

      val filter: Store[Int] => (Int => Boolean) => Store[Int] = {
        store =>
         pre =>
          store.filterKeys(pre).asInstanceOf[Store[Int]]
      }
    }
  } */

  implicit def simpleStore: StoreLike[Int, Store[Int], Clo[Int]] = {

    new StoreLike[Int, Store[Int], Clo[Int]] {

      val apply: Store[Int] = new HashMap[Int, Clo[Int]]

      val bind: Store[Int] => Clo[Int] => Int = {
        store =>
          data =>
            val address = alloc(store)
            store + (address -> data)
            address
      }

      val write: Store[Int] => Int => Clo[Int] => Store[Int] = {
        store =>
          address =>
            clo =>
              store + (address -> clo)
      }

      val read: Store[Int] => Int => Option[Clo[Int]] = {
        store =>
          address =>
            store.get(address)
      }

      val filter: Store[Int] => (Int => Boolean) => Store[Int] = {
        store =>
          pre =>
            store.filterKeys(pre).asInstanceOf[HashMap[Int, Clo[Int]]]
      }
    }
  }
}


trait PureStoreLike[M[_],A,C]{

  val bind: A => C => M[_]

  val write: A => C => M[_]

  val read: A => M[_]

  val alloc: C => M[_]

}

object PureStoreLike {

  implicit def mvarStore: PureStoreLike[Task,Channel[Rho],Rho] = {

    new PureStoreLike[Task,Channel[Rho],Rho] {

      val alloc: Rho => Task[Channel[Rho]] =
        rho => Task { MVar.empty }

      val write: Channel[Rho] => Rho => Task[Unit] =
        ch => rho => ch.put(rho)

      val bind: Channel[Rho] => Rho => Task[Unit] =
        addr => rho => write(addr)(rho)

      val read: Channel[Rho] => Task[Rho] =
        ch => ch.take

    }
  }
}
