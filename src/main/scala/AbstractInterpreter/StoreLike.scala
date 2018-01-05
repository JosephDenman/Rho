package AbstractInterpreter

import AbstractInterpreter.StateSpace.Store

sealed trait StoreLike[A,S,C]{

  val apply: S
  val bind: S => C => A
  val write: S => A => C => S
  val read: S => A => Option[C]
  val filter: S => (A => Boolean) => S
  val alloc: Store[Int] => Int =
    store =>
      store.keys.foldLeft(0){(a,b) => Math.max(a,b)} + 1

}
/*
object StoreLike {

  implicit def abstractStore: StoreLike[Int, Store[Int], Val[Int]] = {

    new StoreLike[Int, Store[Int], Val[Int]]{

      val apply: Store[Int] = new HashMap[Int,Val[Int]]

      /*
        "Joining(⨆) on the abstract store allows each address
        in the finite set of abstract addresses to represent
        multiple concrete addresses....the abstract allocator
        distinguishes between bindings of the same variable in
        different contexts. It determines how many abstract
        variants are associated with a variable."
      */

      val bind: Store[Int] => Val[Int] => Int = {
        store =>
          data =>
            val address = alloc(store)
              ⨆[Int, Val[Int]](store, List(address -> data))
              address
      }

      val write: Store[Int] => Int => Val[Int] => Store[Int] = {
        store =>
          address =>
            data =>
              ⨆[Int, Val[Int]](store, List(address -> data))
      }

      val read: Store[Int] => Int => Option[Val[Int]] = {
        store =>
          address =>
            Some(!![Int, Val[Int]](store).apply(address))
      }

      val filter: Store[Int] => (Int => Boolean) => Store[Int] = {
        store =>
         pre =>
          store.filterKeys(pre).asInstanceOf[Store[Int]]
      }
    }
  }

  implicit def simpleStore: StoreLike[Int, Store[Int], Val[Int]] = {

    new StoreLike[Int, Store[Int], Val[Int]] {

      val apply: Store[Int] = new HashMap[Int, Val[Int]]

      val bind: Store[Int] => Val[Int] => Int = {
        store =>
          data =>
            val address = alloc(store)
            store + (address -> data)
            address
      }

      val write: Store[Int] => Int => Val[Int] => Store[Int] = {
        store =>
          address =>
            clo =>
              store + (address -> clo)
      }

      val read: Store[Int] => Int => Option[Val[Int]] = {
        store =>
          address =>
            store.get(address)
      }

      val filter: Store[Int] => (Int => Boolean) => Store[Int] = {
        store =>
          pre =>
            store.filterKeys(pre).asInstanceOf[HashMap[Int, Val[Int]]]
      }
    }
  }
}*/







