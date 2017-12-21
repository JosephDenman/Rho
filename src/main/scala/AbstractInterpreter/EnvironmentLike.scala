package AbstractInterpreter

import ADT.Rho
import AbstractInterpreter.Aliases.{Channel, Clo, Env, Store}
import monix.eval.{MVar, Task}

import scala.collection.immutable.HashMap

package object Aliases {

  type Var = String

  type Clo[A] = Rho

  type Env[A] = HashMap[Rho,A]

  type Store[A] = HashMap[A,Clo[A]]

  type Channel[A] = MVar[A]

}

sealed trait SimpleEnvironmentLike[A,S,C]{

  val apply: Env[A] = new HashMap[Rho,A]

  val bind: S => Env[A] => Rho => C => (Env[A], S)

  val write: S => Env[A] => Rho => C => (Env[A], S)

  val read: S => Env[A] => Rho => Option[C]

  val filter: Env[A] => (Rho => Boolean) => Env[A]

}

object SimpleEnvironmentLike {

  implicit def simpleEnv[A](implicit storeLike: StoreLike[A, Store[A], Clo[A]]): SimpleEnvironmentLike[A, Store[A], Clo[A]] = {

    new SimpleEnvironmentLike[A, Store[A], Clo[A]] {

      val bind: Store[A] => Env[A] => Rho => Clo[A] => (Env[A], Store[A]) = {
        store =>
          env =>
            rho =>
              data =>
                (env + (rho -> storeLike.bind(store)(data)), store)
      }

      val write: Store[A] => Env[A] => Rho => Clo[A] => (Env[A], Store[A]) = {
        store =>
          env =>
            rho =>
              clo =>
                (env, (env get rho map {
                  storeLike.write(store)(_)(clo)
                }).get)
      }

      val read: Store[A] => Env[A] => Rho => Option[Clo[A]] = {
        store =>
          env =>
            rho =>
              val result = env get rho map {
                storeLike.read(store)(_)
              }
              result match {
                case None => sys.error(s"No data associated with $rho") // the case where we store the continuation //
                case Some(data) => data
              }
      }

      val filter: Env[A] => (Rho => Boolean) => Env[A] = {
        env =>
          pre =>
            env.filterKeys(pre).asInstanceOf[Env[A]]
      }
    }
  }
}

sealed trait PureEnvironmentLike[M[_],A,C] {

  val apply: Env[A] = new HashMap[Rho,A]

  val bind: Env[A] => Rho => A => Env[A]

  val write: Env[A] => Rho => C => M[_]

  val read: Env[A] => Rho => M[_]

  val filter: Env[A] => (Rho => Boolean) => Env[A]

}

object PureEnvironmentLike {

  implicit def mvarEnv: PureEnvironmentLike[Task,Channel[Rho],Clo[Rho]] = {

    new PureEnvironmentLike[Task,Channel[Rho],Clo[Rho]]{

      val bind: Env[Channel[Rho]] => Rho => Clo[Rho] => Env[Channel[Rho]] = {
        env =>
          rho =>
            clo =>
          ???
      }

      val write: Env[Channel[Rho]] => Rho => Clo[Rho] => Task[Unit] = {
        env =>
          rho =>
            clo =>
              val result = env.get(rho)
              result match {
                case None => sys.error(s"No address has been allocated for $rho")
                case Some(ch) => ch.put(clo)
              }
      }

      val read: Env[Channel[Rho]] => Rho => Task[Rho] = {
        env =>
          rho =>
            val result = env.get(rho)
            result match {
              case None => sys.error(s"No address has been allocated for $rho")
              case Some(ch) => ch.take
            }
      }

      val filter: Env[Channel[Rho]] => (Rho => Boolean) => Env[Channel[Rho]] = {
        env =>
          pre =>
            env.filterKeys(pre).asInstanceOf[Env[Channel[Rho]]]
      }
    }
  }
}