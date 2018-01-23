package ADT


import cats.{Applicative, Eval, Foldable, Functor, Traverse}

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

trait Channel extends Serializable

  case class Quote(unquote: Proc[Channel]) extends Channel {
    override def toString: String = "@(" + unquote + ")"
  }

  case class Var(id: String) extends Channel {
    override def toString: String = id
  }

// Term constructors
sealed trait Proc[+Chan] extends Serializable

object Proc {

  // 0 : 1 -> P
  final case class Zero[+Chan]() extends Proc[Chan]{
    override def toString: String = "0"
  }

  // ! : N x P -> P
  final case class Output[+Chan](x: Chan, q: Proc[Chan]) extends Proc[Chan] {
    override def toString: String = x.toString + "!(" + q.toString + ")"
  }

  // for : N x N x P -> P
  final case class Input[+Chan](z: Chan, x: Chan, k: Proc[Chan]) extends Proc[Chan]{
    override def toString: String = "for( " + z.toString + " <- " + x.toString + " ){ " + k.toString + " }"
  }

  // | : P x P -> P
  final case class Par[+Chan](processes: Proc[Chan]* ) extends Proc[Chan]{
    override def toString: String = { processes.map(p => p.toString).mkString(" | ") }
  }

  // * : N -> P
  final case class Drop[+Chan](x: Chan) extends Proc[Chan]{
    override def toString: String = "*" + x.toString
  }

  // Neu : N x P -> P
  final case class New[+Chan](x: Chan, p: Proc[Chan]) extends Proc[Chan] {
    override def toString: String = "new " + x + " in { " + p.toString + " }"
  }

/*
  final case class Fix[F[_] : Functor](unFix: F[Fix[F]])

  type Algebra[F[_],A] = F[A] => A

  type InitAlgebra[F[_]] = Algebra[F[_],Fix[F[_]]]

  type CoAlgebra[F[_],A] = A => F[A]

  type TerminalAlgebra[F[_]] = CoAlgebra[F[_],Fix[F[_]]]

  val drop: TerminalAlgebra[Proc] = { // Fix[Proc[_]] => Proc[Fix[Proc[_]]]
    case Fix(proc) => proc
  }

  val quote: InitAlgebra[Proc] = { // Proc[Fix[Proc[_]]] => Fix[Proc[_]]
    proc => Fix(proc)
  }*/

  implicit val functorProc: Functor[Proc] = new Functor[Proc] {
    def map[A, B](proc: Proc[A])(func: A => B): Proc[B] =
      proc match {
        case Zero() => Zero()
        case Drop(x) => Drop(func(x))
        case Input(z,x,k) => Input(func(z),func(x),map(k)(func))
        case Output(x, p) => Output(func(x), map(p)(func))
        case Par(xs@_*) =>
          val newXs = xs map { p => map(p)(func) }
          Par(newXs: _*)
        case New(x,proc1) => New(func(x), map(proc1)(func))
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
        case New(x,proc1) => foldLeft(proc1,f(b,x))(f)
      }

    def foldRight[A, B](proc: Proc[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      proc match {
        case Zero() => lb
        case Drop(x) => f(x, lb)
        case Input(z,x,k) => f(z,f(x,foldRight(k,lb)(f)))
        case Output(x, p) => f(x, foldRight(p, lb)(f))
        case Par(proc1, proc2) => foldRight(proc1, foldRight(proc2, lb)(f))(f)
        case New(x,proc1) => foldRight(proc1,f(x,lb))(f)
      }
  }

  implicit val traversableProc: Traverse[Proc] = new Traverse[Proc] {

    def traverse[G[_], A, B](proc: Proc[A])(func: A => G[B])(implicit ap: Applicative[G]): G[Proc[B]] =
      proc match {
        case Zero() => ap.pure(Zero())
        case Drop(x) => ap.map(func(x))(Drop[B])
        case Input(z,x,k) => ap.map3(func(z),func(x),traverse(k)(func))(Input[B])
        case Output(x, p) => ap.map2(func(x), traverse(p)(func))(Output[B])
        case Par(proc1, proc2) => ap.map2(traverse(proc1)(func), traverse(proc2)(func))(Par[B](_,_))
        case New(x,proc1) => ap.map2(func(x),traverse(proc1)(func))(New[B])
      }

    def foldLeft[A, B](proc: Proc[A], b: B)(f: (B, A) => B): B =
      foldableProc.foldLeft(proc, b)(f)

    def foldRight[A, B](proc: Proc[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldableProc.foldRight(proc, lb)(f)
  }
}