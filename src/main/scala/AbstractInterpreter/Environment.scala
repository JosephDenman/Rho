package AbstractInterpreter

import ADT.Proc._
import AbstractInterpreter.StoreLike._
import scala.collection.immutable.HashMap

sealed trait EnvironmentLike[A]{

    type Environment[A] = HashMap[ADT.Rho,A]

    val apply : Environment[A]

    val write : (StoreLike[A], Environment[A], Rho) => Environment[A]

    val read : (StoreLike[A], Environment[A], Rho) => Option[A]
}

object EnvironmentLike{

    implicit def simpleEnv[A] : EnvironmentLike[A] = {

        val apply : Environment[A] = new HashMap[Rho, A]

        val write : (StoreLike[A],Environment[A], Rho) => Environment[A]
        = (storeLike, env, proc) =>
          storeLike.write(storeLike.apply, addr, "proof-of-concept")
          env + (name -> addr)

        val read : (StoreLike[A],Environment[A], Rho) => Option[Environment[A]]
        = sys.error("unimplemented")

    }

}