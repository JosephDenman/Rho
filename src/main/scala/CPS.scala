import scala.collection.immutable.Map

case class Lambda(args: List[Var], body: CExp)

sealed trait AExp

  case class Ref(v: Var) extends AExp

  case class Lam(lambda: Lambda) extends AExp

sealed trait CExp

  case class Call(ax: AExp, axs: List[AExp]) extends CExp

  case class Exit() extends CExp

sealed trait Val

  case class Clo(l: Lambda, env: Env) extends Val


type Var = String

type State = (CExp, Env, Store, Time)

type Env = Map[Var,Address]

type Store = Map[Address, Set[Val]]

type Address = (Var,Time)

type Time = List[CExp]

def join[K,V](f: Map[K,Set[V]], list: List[(K,Set[V])]):
Map[K,Set[V]] = list match {
  case Nil => f
  case (k,v) :: tl =>
    val newVal = f.getOrElse(k, Set()) ++ v
    (f - k) + (k -> newVal)
}

def arg(env: Env, store: Store)(aexp: AExp):
Set[Val] = {
  aexp match {
    case Ref(v) => store(env(aexp))
    case Lam(l) => Set(Clo(l,env))
  }
}

val k = 1

def tick(v: Val, s: State):
Time = {
  (v, s) match {
    case (_, (call, _, _, t)) => (call :: t) take k
  }
}

def fun(env: Env, store: Store)(aexp: AExp): List[Val]

def alloc(time: Time, value: Val, state: State)(variable: Var):
Address = (variable,time)

def next(s: State): List[State] = {
  s match {
    case st@(Call(f,aes), p, store, time) =>
      for(proc@Clo(lamb,pPrime) <- arg(p,store)(f).toList;
          timePrime = tick(proc,st) ;
          as = for(v <- lamb.args) yield {
            alloc(timePrime, proc, st)(v)
          } ;
          ds = for(ae <- aes) yield {
            arg(p,store)(ae)
          } ;
          update = for(v <- lamb.args ; a <- as) yield { (v,a) } ;
          updateKeySet = update.map( x => x._1 );
          pDoublePrime = (pPrime -- updateKeySet) ++ update ;
          storeIntermediate = for (a <- as ; d <- ds) yield { (a,d) } ;
          storePrime = join(store, storeIntermediate)
      ) yield {(lamb.body, pDoublePrime, storePrime, timePrime)}
  }
}

val idx = Lam(Lambda(List("x"),Call(Ref("x"),List())))
val idy = Lam(Lambda(List("y"),Exit()))
val comb = Lam(Lambda(List("f","g"), Call(Ref("f"),List(Ref("g")))))
val ex = Call(comb,List(idx,idy))
val ucombx = Lam(Lambda(List("x"),Call(Ref("x"),List(Ref("x")))))
val ucomby = Lam(Lambda(List("y"),Call(Ref("y"),List(Ref("y")))))
val omega = Call(ucombx, List(ucomby))



def free_
def free(aexp: AExp, svar: Set[Var]): Set[Var] =
  aexp match {
    case (Ref(v)) =>
    case (Lam(lambda)) =>
  }