package ADT

import cats.Applicative
import cats.Eval
import cats.Functor
import cats.Foldable
import cats.Traverse

// P[X]

sealed trait Proc[Name]

// X := X[P[X]]
case class Rho(proc: Proc[Rho])

// 0
case class Zero[Name]() extends Proc[Name]

// X!P[X]
case class Output[Name](x: Name, p: Proc[Name]) extends Proc[Name]

// for ( X <- X )P[X]
case class Input[Name](z: Name, x: Name, p: Proc[Name]) extends Proc[Name]

// P[X] | P[X]
case class Par[Name](left: Proc[Name], right: Proc[Name]) extends Proc[Name]

// *X
case class Drop[Name](x: Name) extends Proc[Name]

//
case class Comm[Name](x: Name, q: Proc[Name], kont: Proc[Name])


object Proc {

  implicit val functorProc: Functor[Proc] = new Functor[Proc]{
    def map[A, B](proc: Proc[A])(func: A => B): Proc[B] =
      proc match {
        case Zero() => Zero[B]()
        case Drop(x) => Drop[B](func(x))
        case Input(z,x,p) => Input[B](func(z),func(x),map(p)(func))
        case Output(x,p) => Output[B](func(x), map(p)(func))
        case Par(proc1,proc2) => Par[B](map(proc1)(func), map(proc2)(func))
      }
  }

  implicit val foldableProc: Foldable[Proc] = new Foldable[Proc]{
    def foldLeft[A, B](proc: Proc[A],b: B)(f: (B, A) => B): B =
      proc match {
        case Zero() => b
        case Drop(x) => f(b,x)
        case Input(z,x,p) => f(f(foldLeft(p,b)(f),x),z)
        case Output(x,p) => f(foldLeft(p,b)(f),x)
        case Par(proc1,proc2) => foldLeft(proc2,foldLeft(proc1,b)(f))(f)
      }

    def foldRight[A, B](proc: Proc[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      proc match {
        case Zero() => lb
        case Drop(x) => f(x,lb)
        case Input(z,x,p) => f(x,f(z,foldRight(p,lb)(f)))
        case Output(x,p) => f(x,foldRight(p,lb)(f))
        case Par(proc1,proc2) => foldRight(proc1,foldRight(proc2,lb)(f))(f)
      }
  }

  implicit val traversableProc: Traverse[Proc] = new Traverse[Proc]{

    def traverse[G[_], A, B](proc: Proc[A])(func: A => G[B])(implicit ap: Applicative[G]): G[Proc[B]] =
      proc match {
        case Zero() => ap.pure(Zero[B]())
        case Drop(x) => ap.map(func(x))(Drop[B])
        case Input(z,x,p) => ap.map3(func(z), func(x), traverse(p)(func))(Input[B])
        case Output(x,p) => ap.map2(func(x), traverse(p)(func))(Output[B])
        case Par(proc1,proc2) => ap.map2(traverse(proc1)(func), traverse(proc2)(func))(Par[B])
      }

    def foldLeft[A, B](proc: Proc[A],b: B)(f: (B, A) => B): B =
      foldableProc.foldLeft(proc,b)(f)

    def foldRight[A, B](proc: Proc[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldableProc.foldRight(proc,lb)(f)
  }

  def cost(rho: Rho): Int = {
    val unquotecost: Int = sys.error("implement unquote cost")
    val quotecost: Int = sys.error("implement quote cost")
    val bindcost: Int = sys.error("implement bind cost")
    val writecost: Int = sys.error("implement write cost")
    rho match {
      case Rho(Zero()) => 0
      case Rho(Drop(x)) => unquotecost + cost(x)
      case Rho(Input(z,x,proc)) => bindcost + cost(Rho(proc))
      case Rho(Output(x,proc)) => quotecost + writecost
      case Rho(Par(proc1,proc2)) => cost(Rho(proc1)) + cost(Rho(proc2))
    }
  }
}



/*

Abstract Interpretation:

@ : P x Env -> N
  - takes a closure and returns a reference to that closure, i.e., a name.

* : N -> P x Env
  - evaluates a reference to retrieve a closure

Env : N -> A
  - the contents of names. For free names, the contents of the channel. For bound names, the value bound to the name.

Store : A -> N

Term:
0 : 1 -> P
! : N x P -> P
for : N x P -> P
| : P x P -> P
* : N -> P
COMM : 1 -> P
@ : P -> N

*/

// Grammar for Rho with Concrete State Space //


