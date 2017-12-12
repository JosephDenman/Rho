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

sealed trait StoreLike[A]{

  private val store: Store[A] = new HashMap[A,Data[A]]

  val getStore: Store[A] = store

  val bind: A => Data[A] => Store[A]

  val write: A => Data[A] => Store[A]

  val read: A => Option[Data[A]]

  val filterStore:(A => Boolean) => Store[A]

}

object StoreLike {

  implicit def abstractStore[A]: StoreLike[A] = {

    new StoreLike[A] {

      /*
        "Joining(⨆) on the abstract store allows each address
        in the finite set of abstract addresses to represent
        multiple concrete addresses....the abstract allocator
        distinguishes between bindings of the same variable in
        different contexts. It determines how many abstract
        variants are associated with the variable."
      */

      val bind: A => Data[A] => Store[A]
        = address => data => ⨆[A, Data[A]](getStore, List(address -> data))

      val read: A => Option[Data[A]]
        = address => Some(!![A, Data[A]](getStore).apply(address))

      val write: A => Data[A] => Store[A]
        = address => data => ⨆[A, Data[A]](getStore, List(address -> data))

      val filterStore: (A => Boolean) => Store[A]
        = pre => getStore.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]
    }
  }

  implicit def concreteStore[A]: StoreLike[A] = {

    new StoreLike[A] {

      val bind: A => Data[A] => Store[A] = {
        address => data =>
            val item = read(address)
            item match {
              case None => getStore + (address -> data)
              case Some(prev) => sys.error(s"variable already bound to $prev")
            }
      }

      val read: A => Option[Data[A]]
        = address => getStore.get(address)

      val write: A => Data[A] => Store[A]
        = address => data => getStore + (address -> data)

      val filterStore: (A => Boolean) => Store[A]
        = pre => getStore.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]
    }
  }
}
