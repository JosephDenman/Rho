import scala.collection.immutable.HashMap

case class Output[Ch](x: Ch, p: Proc[Ch]) extends Proc[Ch]{

  def boundNames: HashMap[Ch, Rho]

  def freeNames: HashMap[Ch,Rho]

  override def toString: String = s"$x!($p)"

  override def equals( o: Any ) : Boolean = {
    o match {
      case that : Output[Ch] =>
        x.equals(that.x) && p.equals(that.p)
      case _ =>
        false
    }
  }

  override def hashCode( ) : Int = {
    5 * p.hashCode()
  }
}

object Output {
  def apply[Ch](x: Ch, p: Proc[Ch]): Output[Ch] = {
    new Output(x,p)
  }
  def unapply[Ch](p: Output[Ch])
  : Option[(Ch,Proc[Ch])] = {
    Some(p.x,p.p)
  }
}