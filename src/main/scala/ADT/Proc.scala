package ADT

import cats.Functor

// Term constructors
trait Proc[+Chan] extends Serializable{
  override def toString: String
}

  // 0 : 1 -> P
  case class Zero[Chan]() extends Proc[Chan]{
    override def toString: String = "0"
  }

  // ! : N x P -> P
  case class Output[Chan](x: Chan, q: Proc[Chan]) extends Proc[Chan]{
    override def toString: String = x.toString + "!(" + q.toString + ")"
  }

  // for : N x N x P -> P
  case class Input[Chan](z: Chan, x: Chan, k: Proc[Chan]) extends Proc[Chan]{
    override def toString: String = "for( " + z.toString + " <- " + x.toString + " ){ " + k.toString + " }"
  }

  // | : P x P -> P
  case class Par[Chan](left: Proc[Chan], right: Proc[Chan]) extends Proc[Chan]{
    override def toString: String = left.toString + " | " + right.toString
  }

  // * : N -> P
  case class Drop[Chan](x: Chan) extends Proc[Chan]{
    override def toString: String = "*" + x.toString
  }

  // and the one we hold on faith - New : N x P -> P
  case class New[Chan](x: Chan, p: Proc[Chan]) extends Proc[Chan]{
    override def toString: String = "new " + x + " in { " + p.toString + " }"
  }


/*
 * The uninhabited type.
 */
case class Void(z: Void)

object Void {

  /*
   * Logical reasoning of type 'ex contradictione sequitur quodlibet'
   */

  def absurd[A](z: Void): A = absurd(z)

  def vacuous[F[_], A](fa: F[Void], z: Void)(implicit F: Functor[F]): F[A] = F.map(fa)(absurd(z))

  // implicit def voidSemiGroup: Semigroup[Void] = new Semigroup[Void] {
  //   def append(f1: Void, f2: => Void) = f2 //right biased
  // }
}

/* trait Chan[A, B]
  //case class Var[A,B](chan:A) extends Chan[A,B]
 // case class Quote[A,B](proc:B) extends Chan[A,B]

object Chan { }

  implicit def functorChan[A]: Functor[Chan[A,?]] = new Functor[Chan[A,?]]{
    def map[B,D](chan: Chan[A,B])(f:B => D): Chan[A,D]
      = chan match {
      case Var(ch) => Var(ch)
      case Quote(proc) => Quote(f(proc))
    }
  }

  implicit def bifunctorChan: Bifunctor[Chan] = new Bifunctor[Chan] {
    def bimap[A, B, C, D](chan: Chan[A,B])
                         (f: A => C, g: B => D): Chan[C, D]
    = chan match {
      case Var(ch) => Var(f(ch))
      case Quote(proc) => Quote(g(proc))
    }
  }

  implicit def bifoldableChan: Bifoldable[Chan] = new Bifoldable[Chan] {
    def bifoldLeft[A, B, C](fab: Chan[A, B], c: C)
                           (f: (C, A) => C, g: (C, B) => C): C
      = fab match {
        case Var(ch) => f(c,ch)
        case Quote(proc) => g(c,proc)
      }
    def bifoldRight[A, B, C](fab: Chan[A, B], c: Eval[C])
                            (f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]
      = fab match {
        case Var(ch) => f(ch,c)
        case Quote(proc) => g(proc,c)
      }
  }

  implicit def foldableChan[Ch]: Foldable[Chan[Ch,?]] = new Foldable[Chan[Ch,?]] {
    def foldLeft[A, B](fa: Chan[Ch,A], b: B)(f: (B, A) => B): B
    = fa match {
      case Var(ch) => b
      case Quote(proc) => f(b,proc)
    }
    def foldRight[A, B](fa: Chan[Ch,A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
    = fa match {
      case Var(ch) => lb
      case Quote(proc) => f(proc,lb)
    }
  }

  implicit def bitraverseChan: Bitraverse[Chan] = new Bitraverse[Chan]{
    def bifoldLeft[A, B, C](fab: Chan[A, B], c: C)
                           (f: (C, A) => C, g: (C, B) => C): C
      = Chan.bifoldableChan.bifoldLeft(fab,c)(f,g)
    def bifoldRight[A, B, C](fab: Chan[A, B], c: Eval[C])
                            (f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]
      = Chan.bifoldableChan.bifoldRight(fab,c)(f,g)
    def bitraverse[G[_], A, B, C, D](fab: Chan[A, B])
                                    (f: (A) => G[C], g: (B) => G[D])
                                    (implicit arg0: Applicative[G]): G[Chan[C, D]]
      = fab match {
        case Var(ch) => arg0.map (f(ch)) (Var(_))
        case Quote(proc) => arg0.map (g(proc)) (Quote(_))
      }
  }

  implicit def traverseChan[Var]: Traverse[Chan[Var,?]] = new Traverse[Chan[Var,?]]{
    def foldLeft[A, B](fa: Chan[Var,A], b: B)(f: (B, A) => B): B
    = Chan.foldableChan.foldLeft(fa,b)(f)
    def foldRight[A, B](fa: Chan[Var,A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
    = Chan.foldableChan.foldRight(fa,lb)(f)
    def traverse[G[_], A, B](fa: Chan[Var,A])(f: (A) => G[B])(implicit arg0: Applicative[G]): G[Chan[Var,B]]
    = fa match {
      case Var(x) => arg0.pure(Var(x))
      case Quote(proc) => arg0.map (f(proc)) (Quote(_))
    }
  }

}

case class Scope[F[_],A,B](unscope: F[Chan[A,F[B]]])

object Scope {

  implicit def bifunctorScope[F[_]](implicit F: Functor[F]): Bifunctor[Scope[F, ?, ?]] = new Bifunctor[Scope[F, ?, ?]] {
    def bimap[A, B, C, D](scope: Scope[F, A, B])(fa: A => C, fb: B => D): Scope[F, C, D] = {
      Scope[F, C, D](F.map(scope.unscope)(Chan.bifunctorChan.bimap(_)(fa, F.map(_)(fb))))
    }
  }

  implicit def functorScope[F[_],A](implicit F: Functor[F]): Functor[Scope[F, A, ?]] = new Functor[Scope[F, A, ?]] {
    def map[B, D](scope: Scope[F, A, B])(f: B => D): Scope[F, A, D] = bifunctorScope.bimap[A, B, A, D](scope)(identity, f)
  }

  implicit def bifoldableScope[F[_]](implicit F: Foldable[F]): Bifoldable[Scope[F, ?, ?]] = new Bifoldable[Scope[F,?,?]] {
    def bifoldLeft[A, B, C]
      (fab: Scope[F, A, B], c: C)
      (f: (C, A) => C, g: (C, B) => C): C
      = F.foldLeft (fab.unscope,c) ((acc,ch) => Chan.bifoldableChan.bifoldLeft(ch,acc)(f,(cc,fb) => F.foldLeft (fb,cc) (g)))
    def bifoldRight[A, B, C]
      (fab: Scope[F, A, B], c: Eval[C])
      (f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]
      = F.foldRight (fab.unscope,c) ((ch,acc) => Chan.bifoldableChan.bifoldRight(ch,acc)(f,(cc,fb) => F.foldRight (cc,fb) (g)))
  }

  implicit def bitraverseScope[F[_]](implicit F: Traverse[F]): Bitraverse[Scope[F,?,?]] = new Bitraverse[Scope[F,?,?]]{
    def bifoldLeft[A, B, C]
      (fab: Scope[F, A, B], c: C)
      (f: (C, A) => C, g: (C, B) => C): C
      = Scope.bifoldableScope.bifoldLeft(fab,c)(f,g)
    def bifoldRight[A, B, C]
      (fab: Scope[F, A, B], c: Eval[C])
      (f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]
      = Scope.bifoldableScope.bifoldRight(fab,c)(f,g)
    def bitraverse[G[_], A, B, C, D]
      (fab: Scope[F,A, B])
      (f: (A) => G[C], g: (B) => G[D])
      (implicit arg0: Applicative[G]): G[Scope[F, C, D]]
      = arg0.map (F.traverse (fab.unscope) (chan => Chan.bitraverseChan.bitraverse (chan) (f,F.traverse (_) (g)))) (Scope(_))
  }

  implicit def scopeFlatten[F[_],A, B, C](scope: Scope[F, A, B])(f: B => F[C])(implicit F: Monad[F]): Scope[F, A, C] = {
    val m = scope.unscope
    Scope[F, A, C](F.flatMap(m)({
      case Var(ch) => F.pure(Var[A, F[C]](ch))
      case Quote(proc) => F.map(F.map(proc)(f))(Quote(_))
    }))
  }

  implicit def abstracT0[F[_],A,B](p: F[A])(f:A => Option[B])(implicit F: Monad[F]): Scope[F,B,A] = {
    val k: A => Chan[B,F[A]] = y => {
      f(y) match {
        case Some(z) => Var(z)
        case None => Quote(F.pure(y))
      }
    }
    Scope[F,B,A](F.map(p)(k))
  }

  implicit def abstracT1[F[_],A](a: A)(p: F[A])(implicit F: Monad[F]): Scope[F,Unit,A] = {
    Scope.abstracT0[F,A,Unit](p)(b => if(a == b) Some(Unit) else None)
  }
}

  implicit val functorProc: Functor[Proc] = new Functor[Proc] {
    def map[A, B](proc: Proc[A])(func: A => B): Proc[B] =
      proc match {
        case Zero() => Zero()
        case Drop(x) => Drop(func(x))
        case Input(x, p) => sys.error("unimplemented")
        case Output(x, p) => Output(func(x), map(p)(func))
        case Par(proc1, proc2) => Par(map(proc1)(func), map(proc2)(func))
      }
  }

  implicit val foldableProc: Foldable[Proc] = new Foldable[Proc] {
    def foldLeft[A, B](proc: Proc[A], b: B)(f: (B, A) => B): B =
      proc match {
        case Zero() => b
        case Drop(x) => f(b, x)
       // case Input(x, p) => sys.error("unimplemented")
        case Output(x, p) => f(foldLeft(p, b)(f), x)
        case Par(proc1, proc2) => foldLeft(proc2, foldLeft(proc1, b)(f))(f)
      }

    def foldRight[A, B](proc: Proc[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      proc match {
        case Zero() => lb
        case Drop(x) => f(x, lb)
       // case Input(x, p) => sys.error("unimplemented")
        case Output(x, p) => f(x, foldRight(p, lb)(f))
        case Par(proc1, proc2) => foldRight(proc1, foldRight(proc2, lb)(f))(f)
      }
  }

  implicit val traversableProc: Traverse[Proc] = new Traverse[Proc] {

    def traverse[G[_], A, B](proc: Proc[A])(func: A => G[B])(implicit ap: Applicative[G]): G[Proc[B]] =
      proc match {
        case Zero() => ap.pure(Zero[B]())
        case Drop(x) => ap.map(func(x))(Drop[B])
       // case Input(x, p) => sys.error("unimplemented")
        case Output(x, p) => ap.map2(func(x), traverse(p)(func))(Output[B])
        case Par(proc1, proc2) => ap.map2(traverse(proc1)(func), traverse(proc2)(func))(Par[B])
      }

    def foldLeft[A, B](proc: Proc[A], b: B)(f: (B, A) => B): B =
      foldableProc.foldLeft(proc, b)(f)

    def foldRight[A, B](proc: Proc[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldableProc.foldRight(proc, lb)(f)
  }*/


/*
Concrete State Space:

State := P x Env x Store
  - states are represented as triplets

Val := P x Env
  - the values sent and received are closures

@ : P x Env -> Chan
  - takes a closure and returns a channel

* : Chan -> P x Env
  - dereferences a channel retrieve it's closure

Env : Var -> A
  - an environment is a finite mapping of free variables to addresses

Store : A -> Chan
  - the store is a finite mapping from addresses to channels

COMM : P x Env x Store -> P' x Env' x Store'

- current work focuses on refactoring the above to have the store map to
a channel queue, where readers and writers are stored. Once that's done,
we apply structural abstraction to derive an abstract state space, yielding
a formal definition of an abstract analysis framework.

*/
