package AbstractInterpreter

import AbstractInterpreter.Aliases.{Data, Store}
import AbstractInterpreter.Lattice._

import scala.collection.immutable.HashMap

// object StoreLike{
//   type Data[A] = String
//   type Store[A] = HashMap[A,Data[A]]

//   val simpleApply: Store[A] = new HashMap[A, Data[A]]
//   val simpleBind: (Store[A], A, Data[A]) => Store[A]
//   = (store: Store[A], address: A, data: Data[A]) => ⨆[A, Data[A]](store, List(address -> data))
//   val simpleRead: (Store[A], A) => Option[Data[A]]
//   = (store: Store[A], address: A) => Some(!![A, Data[A]](store).apply(address))
//   val simpleWrite: (Store[A], A, Data[A]) => Store[A]
//   = (store: Store[A], address: A, data: Data[A]) => ⨆[A, Data[A]](store, List(address -> data))
//   val simpleFilterStore: (Store[A], A => Boolean) => Store[A]
//   = (store: Store[A], pre: A => Boolean) => store.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]

//   val smartApply: Store[A] = new HashMap[A, Data[A]]

//   val smartBind: (Store[A], A, Data[A]) => Store[A]
//     = (store: Store[A], address: A, data: Data[A]) => {
//       val item: Option[Data[A]] = store.get(address)
//       item match {
//         case None => store + (address -> data)
//         case Some(prev) => sys.error("reassignment to val")
//       }
//     }

//   val smartRead: (Store[A], A) => Option[Data[A]]
//     = (store: Store[A], address: A) => {
//     store.get(address)
//     }

//   val smartWrite: (Store[A], A, Data[A]) => Store[A]
//     = (store: Store[A], address: A, data: Data[A]) => store + (address -> data)

//   val smartFilterStore: (Store[A], A => Boolean) => Store[A]
//     = (store: Store[A], pre: A => Boolean) => store.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]

// }

sealed trait StoreLike[A,S,D]{

  val apply: S

  val bind: S => D => A

  val write: S => A => D => S

  val read: S => A => Option[D]

  val filter: S => (A => Boolean) => S

  val alloc: Store[Int] => Int = {
    store =>
      store.keys.foldLeft(0){(a,b) => Math.max(a,b)} + 1
  }
}

object StoreLike {

  implicit def abstractStore: StoreLike[Int, Store[Int], Data[Int]] = {

    new StoreLike[Int, Store[Int], Data[Int]]{

      val apply: Store[Int] = new HashMap[Int,Data[Int]]

      /*
        Joining(⨆) on the abstract store allows each address
        in the finite set of abstract addresses to represent
        multiple concrete addresses....the abstract allocator
        distinguishes between bindings of the same variable in
        different contexts. It determines how many abstract
        variants are associated with a variable.
      */

      val bind: Store[Int] => Data[Int] => Int = {
        store =>
          data =>
            val address = alloc(store)
              ⨆[Int, Data[Int]](store, List(address -> data))
              address
      }

      val write: Store[Int] => Int => Data[Int] => Store[Int] = {
        store =>
          address =>
            data =>
              ⨆[Int, Data[Int]](store, List(address -> data))
      }

      val read: Store[Int] => Int => Option[Data[Int]] = {
        store =>
          address =>
            Some(!![Int, Data[Int]](store).apply(address))
      }

      val filter: Store[Int] => (Int => Boolean) => Store[Int] = {
        store =>
         pre =>
          store.filterKeys(pre).asInstanceOf[Store[Int]]
      }
    }
  }

  implicit def concreteStore: StoreLike[Int, Store[Int], Data[Int]] = {

    new StoreLike[Int, Store[Int], Data[Int]] {

      val apply: Store[Int] = new HashMap[Int, Data[Int]]

      val bind: Store[Int] => Data[Int] => Int = {
        store =>
          data =>
            val address = alloc(store)
            store + (address -> data)
            address
      }

      val write: Store[Int] => Int => Data[Int] => Store[Int] = {
        store =>
          address =>
            data =>
              store + (address -> data)
      }

      val read: Store[Int] => Int => Option[Data[Int]] = {
        store =>
          address =>
            store.get(address)
      }

      val filter: Store[Int] => (Int => Boolean) => Store[Int] = {
        store =>
          pre =>
            store.filterKeys(pre).asInstanceOf[HashMap[Int, Data[Int]]]
      }
    }
  }
}
