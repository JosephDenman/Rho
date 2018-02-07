package AbstractInterpreter

import ADT.{Name, Proc}
import AbstractInterpreter.StateSpace.{Env, Store}
import monix.eval.MVar

import scala.collection.immutable.HashMap

package object StateSpace {

  type Env[A] = HashMap[Name,A]
  type Store[A] = HashMap[A,StoreChan]

}

case class State[A](heap: Store[A], env: Env[A], exp: Proc)

case class Cont[A](env: Env[A], proc: Proc)

case class StoreChan(mv: MVar[Name], clo: Cont[_])

case class IOAddr(var value: Option[StoreChan]){
  def lookup: StoreChan = value match {
    case Some(data) => data
    case None => sys.error("Null pointer exception")
  }
}