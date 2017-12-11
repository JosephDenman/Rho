package AbstractInterpreter

import AbstractInterpreter.Lattice._
import scala.collection.immutable.HashMap

  sealed trait StoreLike[A]{

    type Data[Address] = String

    type Store[Address] = HashMap[A,Data[Address]]

    val init: Store[A]

    val bind:(Store[A], A, Data[A]) => Store[A]

    val write:(Store[A], A, Data[A]) => Store[A]

    val read:(Store[A], A) => Data[A]

    val filterStore:(Store[A], A => Boolean) => Store[A]

  }

/*type Var = String

type Address = String

type Env[A] = HashMap[Var,A] */

  object StoreLike {

    import AbstractInterpreter.Lattice

    implicit def smartStore[A]: StoreLike[A] = {

      new StoreLike[A] {

        override val init: Store[A] = new HashMap[A, Data[A]]

        val bind: (Store[A], A, Data[A]) => Store[A]
        = (store: Store[A], address: A, data: Data[A]) => ⨆[A, Data[A]](store, List(address -> data))

        val read: (Store[A], A) => Data[A]
        = (store: Store[A], address: A) => !![A, Data[A]](store).apply(address)

        val write: (Store[A], A, Data[A]) => Store[A]
        = (store: Store[A], address: A, data: Data[A]) => ⨆[A, Data[A]](store, List(address -> data))

        val filterStore: (Store[A], A => Boolean) => Store[A]
        = (store: Store[A], pre: A => Boolean) => store.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]
      }
    }

    implicit def simpleStore[A]: StoreLike[A] = {

      new StoreLike[A] {

        override val init: Store[A] = new HashMap[A, Data[A]]

        val bind: (Store[A], A, Data[A]) => Store[A]
          = (store: Store[A], address: A, data: Data[A]) => {
            val item: Option[Data[A]] = store.get(address)
            item match {
              case None => store + (address -> data)
              case Some(prev) => sys.error("reassignment to val")
            }
        }

        val read: (Store[A], A) => Data[A]
          = (store: Store[A], address: A) => {
          val item: Option[Data[A]] = store.get(address)
          item match {
            case None => _
            case Some(prev) => prev
          }
        }

        val write: (Store[A], A, Data[A]) => Store[A]
          = (store: Store[A], address: A, data: Data[A]) => store + (address -> data)

        val filterStore: (Store[A], A => Boolean) => Store[A]
          = (store: Store[A], pre: A => Boolean) => store.filterKeys(pre).asInstanceOf[HashMap[A, Data[A]]]
      }
    }
  }


  /*

   Register
      |
      V
   Memory - Address
      |
      V
   Disk Storage - Keccak256 Address
      |
      V
   Network - TCP/IP

 */

/*
  trait Store[A] {
    val store = new TrieMap[A, Context[A]]()
    def putStore(key: A, value: Context[A]): Unit = {
      store.update(key, value)
    }
    def getStore(key: A): Option[Context[A]] = {
      store.get(key)
    }
  }

  object Store {

  }

  trait Env[A] {
    val env: TrieMap[Var, A]
    def putEnv(key: Var, value: Context[A]): Unit
    def getEnv(key: Var): Option[Context[A]]
  }

  class ProcEnv[A] extends Env[A] {

    val env = new TrieMap[Var, A]()

    def putEnv(key: Var, value: Context[A]): Unit = {
      val intermediate = env.get(key)
      intermediate match {
        case None => sys.error("No address associated with variable")
        case Some(addr) => Store.putStore(addr, value)
        // putStore(addr, value)
      }
    }

    def getEnv(key: Var): Option[Context[A]] = {
      val intermediate = env.get(key)
      intermediate match {
        case None => sys.error("No address associated with variable")
        case Some(addr) => Store.getStore(addr)
      }
    }
}


// * Var -> Address  Address -> Context[Address]
// @ Context[Address] -> Address   Address -> Var

/* x!(Q) | for( @Q <- x ){ @Q!(P) | for( y <- @Q ){ *y } } */




*/