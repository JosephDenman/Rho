package AbstractInterpreter

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

  type Data[A] = String

  type Store = HashMap[A, Data[A]]

  private val globalStore: Store = new HashMap[A,Data[A]]

  val getStore: Store = globalStore

  val bind:(A, Data[A]) => Store

  val write:(A, Data[A]) => Store

  val read: A => Option[Data[A]]

  val filterStore:(A => Boolean) => Store

}

object StoreLike {

  implicit def smartStore[A]: StoreLike[A] = {

    new StoreLike[A] {

      val bind: (A, Data[A]) => Store
        = (address: A, data: Data[A]) => ⨆[A, Data[A]](getStore, List(address -> data))

      val read: A => Option[Data[A]]
        = (address: A) => Some(!![A, Data[A]](getStore).apply(address))

      val write: (A, Data[A]) => Store
        = (address: A, data: Data[A]) => ⨆[A, Data[A]](getStore, List(address -> data))

      val filterStore: (A => Boolean) => Store
        = (pre: A => Boolean) => getStore.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]
    }
  }

  implicit def simpleStore[A]: StoreLike[A] = {

    new StoreLike[A] {
      val bind: (A, Data[A]) => HashMap[A, Data[A]]
        = (address: A, data: Data[A]) => {
          val item: Option[Data[A]] = read(address)
          item match {
            case None => getStore + (address -> data)
            case Some(prev) => sys.error(s"variable already bound to $prev")
          }
      }

      val read: (A) => Option[Data[A]]
        = (address: A) => getStore.get(address)

      val write: (A, Data[A]) => Store
        = (address: A, data: Data[A]) => getStore + (address -> data)

      val filterStore: (A => Boolean) => Store
        = (pre: A => Boolean) => getStore.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]
    }
  }
}
