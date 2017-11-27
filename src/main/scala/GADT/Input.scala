import scala.collection.immutable.HashMap

case class Input[Ch](z: Ch, x: Ch, p: Proc[Ch]) extends Proc[Ch]{

  def boundNames: HashMap[Ch, Rho]

  def freeNames: HashMap[Ch, Rho]

  override def toString: String = s"for($z <- $x){$p}"

  override def equals( o: Any ) : Boolean = {
    o match {
      case that : Input[Ch] =>
        z.equals(that.z) && p.equals(that.p) && x.equals(that.x)
      case _ =>
        false
    }
  }

  override def hashCode( ) : Int = {
    7 * p.hashCode()
  }
}

object Input {
  def apply[Ch]( z: Ch, x: Ch, p: Proc[Ch]): Input[Ch] = {
    new Input(z,x,p)
  }
  def unapply[Ch](p: Input[Ch]): Option[(Ch,Ch,Proc[Ch])] ={
    Some(p.z, p.x, p.p)
  }
}