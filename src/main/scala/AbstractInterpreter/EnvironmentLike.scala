package AbstractInterpreter

import ADT.Rho
import AbstractInterpreter.Aliases.{Data, Env, Store}

import scala.collection.immutable.HashMap

package object Aliases {

  type Var = String

  type Data[A] = String

  type Env[A] = HashMap[Rho,A]

  type Store[A] = HashMap[A,Data[A]]

}

sealed trait EnvironmentLike[A,S,D]{

  val apply: Env[A] = new HashMap[Rho,A]

  val write: StoreLike[A,S,D] => Env[A] => Rho => D => (Env[A],Store[A])

  val read: StoreLike[A,S,D] => Env[A] => Rho => Option[D]

  val filter: Env[A] => (Rho => Boolean) => Env[A]

}

object EnvironmentLike2 {

  implicit def simpleEnv[A](implicit storeLike: StoreLike[A, Store[A],Data[A]]): EnvironmentLike[A,Store[A],Data[A]] = {

    new EnvironmentLike[A,Store[A],Data[A]] {

      val write: Store[A] => Env[A] => Rho => Data[A] => (Env[A],Store[A]) = {
        store =>
          env =>
            rho =>
              data =>
                (env, (env get rho map {storeLike.write(store)(_)(data)}).get)
      }

      val read: Store[A] => Env[A] => Rho => Option[Data[A]] = {
        store =>
          env =>
            rho =>
              val result = env get rho map {storeLike.read(store)(_)}
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


/*

CEK:
( var , env , store, kont ) -> ( val, env', store, kont ) where store(env(var)) = ( val , env' )

( (exp0, exp1), env, store, kont ) -> ( exp0, env, store, Ar( exp1, env, kont ) )

( val, env, store, Ar ( exp, env', kont ) ) -> ( exp , env', store, fun( val, env, kont ) )

( val, env, store, fun( λx.e, env', kont ) -> ( e, env'[ x -> a ], store[ a -> (val,env) ] , kont ) where !store.contains(a)



RHO:
read : Env x Chan -> Env x Chan

( name, env ) -> ( data, env' ) where env(name) = (data, env')


bind : Var x Env x Store x Chan -> Env x Store

( data, env, var ) ->  env [ var -> (data, env) ]


write : Env x Var x Proc -> Env

( env, var, (Q,env') ) -> env [ var -> (Q,env') ]


input : Env x Chan x Var x Kont -> Env x Proc

( env, proc(name, var, kont) -> ( env', kont) where env' = bind ( var, read env name )

 */