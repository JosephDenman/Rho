package AbstractInterpreter

import ADT.Rho
import AbstractInterpreter.Aliases.{Data, Environment}

import scala.collection.immutable.HashMap

package object Aliases {

  type Data[A] = String

  type Environment[A] = HashMap[Rho,A]

  type Store[A] = HashMap[A,Data[A]]
}

sealed trait EnvironmentLike[A]{

    val apply: Environment[A] = new HashMap[Rho,A]

    val write: Environment[A] => Rho => Data[A] => Environment[A]

    val read: Environment[A] => Rho => Option[Data[A]]
}

object EnvironmentLike {

    implicit def simpleEnv[A](implicit store: StoreLike[A]): EnvironmentLike[A] = {

      new EnvironmentLike[A] {

        val write: Environment[A] => Rho => Data[A] => Environment[A] = {
          env => x => Q => env get x map {store.write(_)(Q)}
            env
        }

        val read: Environment[A] => Rho => Option[Data[A]] = {
          env => x => {
            val result = env get x map {store.read(_)}
              result match {
                case None => sys.error(s"No data associated with $x") // the case where we store the continuation //
                case Some(data) => data
              }
          }
        }
      }
    }
}

/*

Read : Env x Chan -> Chan

Bind : Env x Var x Chan -> Env

Input : Env x Kont x Chan x Var -> Env x Kont

 */