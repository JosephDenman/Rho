import cats.Functor

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

implicit val functorForProc: Functor[Proc] = new Functor[Proc]{
  def map[A,B](proc: Proc[A])(func: A => B): Proc[B] =
    proc match {
      case Zero() => Zero[B]()
      case Drop(x) => Drop[B](func(x))
      case Input(z,x,p) => Input[B](func(z),func(x),map(p)(func))
      case Output(x,p) => Output[B](func(x), map(p)(func))
      case Par(proc1,proc2) => Par[B](map(proc1)(func), map(proc2)(func))
    }
}

def cost(rho: Rho): Int = {
  rho match {
    case Rho(Zero()) => 0
    case Rho(Drop(x)) => cost(unquote) + cost(x)
    case Rho(Input(z,x,proc)) => cost(bind) + cost(Rho(proc))
    case Rho(Output(x,proc)) => cost(quote) + cost(write)
    case Rho(Par(proc1,proc2)) => cost(Rho(proc1)) + cost(Rho(proc2))
  }
}


/*

0 : 1 -> P
! : N x P -> P
for : N x P -> P
| : P x P -> P
* : N -> P
COMM : 1 -> P

*/

// Grammar for Rho with Concrete State Space //

