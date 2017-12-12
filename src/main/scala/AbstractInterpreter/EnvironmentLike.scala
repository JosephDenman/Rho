package AbstractInterpreter

import ADT.Rho

import scala.collection.immutable.HashMap

sealed trait EnvironmentLike[A]{

    type Data = String

    type Environment = HashMap[Rho,A]

    type Store = HashMap[A,Data]

    val apply: Environment

    val write: (StoreLike[A], Environment, Rho, Data) => Environment

    val read: (StoreLike[A], Environment, Rho) => Option[Data]
}

// introduce implicit parameter environment or environmentLike //

object EnvironmentLike {

    implicit def simpleEnv[A] : EnvironmentLike[A] = {

      new EnvironmentLike[A] {

        val apply: Environment = new HashMap[Rho,A]

        val write: (StoreLike[A], Environment, Rho, Data) => Environment = {
          (storeLike, env, proc, data) => {
            val address = env get proc
            address match {
              case None => sys.error(s"No address associated with $proc")
              case Some(a) => storeLike.write(a, data)
            }
          }
            env
        }

        val read: (StoreLike[A], Environment, Rho) => Option[Data] = {
          (storeLike, env, proc) => {
            val address = env get proc
              address match {
                case None => sys.error(s"No address associated with $proc")
                case Some(a) => storeLike.read(a)
              }
          }
        }

      }
    }
}