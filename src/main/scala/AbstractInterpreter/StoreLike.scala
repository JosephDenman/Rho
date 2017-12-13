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

  val bind: S => A => D => S

  val write: S => A => D => S

  val read: S => A => Option[D]

  val filter: S => (A => Boolean) => S

}

object StoreLike2 {

  implicit def abstractStore2[A]: StoreLike[A,Store[A],Data[A]] = {

    new StoreLike[A, Store[A], Data[A]]{

      val apply: Store[A] = new HashMap[A,Data[A]]

      /*
        Joining(⨆) on the abstract store allows each address
        in the finite set of abstract addresses to represent
        multiple concrete addresses....the abstract allocator
        distinguishes between bindings of the same variable in
        different contexts. It determines how many abstract
        variants are associated with a variable.
      */

      val bind: Store[A] => A => Data[A] => Store[A] = {
        store =>
          address =>
            data =>
              ⨆[A, Data[A]](store, List(address -> data))
      }

      val read: Store[A] => A => Option[Data[A]] = {
        store =>
          address =>
            Some(!![A, Data[A]](store).apply(address))
      }

      val write: Store[A] => A => Data[A] => Store[A] = {
        store =>
         address =>
          data =>
            ⨆[A, Data[A]](store, List(address -> data))
      }

      val filter: Store[A] => (A => Boolean) => Store[A] = {
        store =>
         pre =>
          store.filterKeys(pre).asInstanceOf[Store[A]]
      }
    }
  }

  implicit def concreteStore2[A]: StoreLike[A,Store[A],Data[A]] = {

    new StoreLike[A, Store[A], Data[A]] {

      val apply: Store[A] = new HashMap[A, Data[A]]

      val bind: Store[A] => A => Data[A] => Store[A] = {
        store =>
          address =>
            data =>
              val item = read(store)(address)
              item match {
                case None => store + (address -> data)
                case Some(prev) => sys.error(s"variable already bound to $prev")
              }
      }

      val read: Store[A] => A => Option[Data[A]] = {
        store =>
          address =>
            store.get(address)
      }

      val write: Store[A] => A => Data[A] => Store[A] = {
        store =>
          address =>
            data =>
              store + (address -> data)
      }

      val filter: Store[A] => (A => Boolean) => Store[A] = {
        store =>
          pre =>
            store.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]
      }
    }
  }
}
