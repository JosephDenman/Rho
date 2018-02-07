package ADT

trait Name extends Serializable

  case class Quote(proc: Proc) extends Name

  case class Var(id: String) extends Name

// Term constructors
trait Proc extends Serializable {
  override def toString: String
}

  // 0 : 1 -> P
  case object Zero extends Proc{
    override def toString: String = "0"
  }

  // ! : N x P -> P
  case class Output(x: Name, send: Send, q: Proc) extends Proc {
    override def toString: String = x.toString + "!(" + q.toString + ")"
  }

  // for : N x N x P -> P
  case class Input(rec: Receipt, k: Proc) extends Proc{
    override def toString: String =  rec.toString + "{ " + k.toString + " }"
  }

  // | : P x P -> P
  case class Par(processes: Proc* ) extends Proc{
    override def toString: String = { processes.map(p => p.toString).mkString(" | ") }
  }

  // * : N -> P
  case class Drop(x: Name) extends Proc {
    override def toString: String = "*" + x.toString
  }

  // and the one we hold on faith - New : N x P -> P
  case class New(x: Var, p: Proc) extends Proc {
    override def toString: String = "new " + x + " in { " + p.toString + " }"
  }

sealed trait Receipt extends Serializable

  case class Bind(z: Name, x: Name) extends Receipt


sealed trait Send extends Serializable

  case object SendSingle extends Send

  case object SendMultiple extends Send



object Proc {

  /*implicit val functorProc: Functor[Proc] = new Functor[Proc] {
    def map[A, B](proc: Proc[A])(func: A => B): Proc[B] =
      proc match {
        case Zero() => Zero()
        case Drop(x) => Drop(func(x))
        case Input(z,x,k) => Input(func(z),func(z),map(k)(func))
        case Output(x, p) => Output(func(x), map(p)(func))
        case Par(proc1, proc2) => Par(map(proc1)(func), map(proc2)(func))
      }
  }

  implicit val foldableProc: Foldable[Proc] = new Foldable[Proc] {
    def foldLeft[A, B](proc: Proc[A], b: B)(f: (B, A) => B): B =
      proc match {
        case Zero() => b
        case Drop(x) => f(b, x)
        case Input(z,x,k) => foldLeft(k,f(f(b,z),x))(f)
        case Output(x, p) => f(foldLeft(p, b)(f), x)
        case Par(proc1, proc2) => foldLeft(proc2, foldLeft(proc1, b)(f))(f)
      }

    def foldRight[A, B](proc: Proc[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      proc match {
        case Zero() => lb
        case Drop(x) => f(x, lb)
        case Input(z,x,k) => f(z,f(x,foldRight(k,lb)(f)))
        case Output(x, p) => f(x, foldRight(p, lb)(f))
        case Par(proc1, proc2) => foldRight(proc1, foldRight(proc2, lb)(f))(f)
      }
  }

  implicit val traversableProc: Traverse[Proc] = new Traverse[Proc] {

    def traverse[G[_], A, B](proc: Proc[A])(func: A => G[B])(implicit ap: Applicative[G]): G[Proc[B]] =
      proc match {
        case Zero() => ap.pure(Zero[B]())
        case Drop(x) => ap.map(func(x))(Drop[B])
        case Input(z,x,k) => ap.map3(func(z),func(x),traverse(k)(func))(Input[B])
        case Output(x, p) => ap.map2(func(x), traverse(p)(func))(Output[B])
        case Par(proc1, proc2) => ap.map2(traverse(proc1)(func), traverse(proc2)(func))(Par[B](_,_))
      }

    def foldLeft[A, B](proc: Proc[A], b: B)(f: (B, A) => B): B =
      foldableProc.foldLeft(proc, b)(f)

    def foldRight[A, B](proc: Proc[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldableProc.foldRight(proc, lb)(f)
  } */
}


/*
Concrete State Space:

COMM : State -> State'

State := P x Env x Store
  - states are represented as triplets

Val := P x Env
  - values sent and received are closures

Store : A -> Chan
  - a finite mapping from addresses to channels

Env : Var -> A
  - a finite mapping of free variables to addresses

Var := An infinite set of identifiers

@ : P x Env -> Chan
  - converts a closure into a channel

* : Chan -> P x Env
  - converts a channel into the original closure

- current work focuses on refactoring the above to have the store map to
a channel queue, where readers and writers are stored. Once that's done,
we apply structural abstraction to derive an abstract state space, yielding
a formal definition of an abstract analysis framework.

*/
