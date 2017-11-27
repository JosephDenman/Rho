import scala.collection.immutable.HashMap

case class Drop[Ch](x: Ch) extends Proc[Ch] {

  def boundNames: HashMap[Ch, Proc[Ch]]

  def freeNames: HashMap[Ch, Proc[Ch]]

  override def toString: String = s"*$x"

  override def equals( o: Any ) : Boolean = {
    o match {
      case that : Drop[Ch] =>
        x.equals(that.x)
      case _ =>
        false
    }
  }

  override def hashCode( ) : Int = {
    11 * x.hashCode()
  }
}

object Drop {
  def apply[Ch](x: Ch): Drop[Ch] = {
    new Drop(x)
  }
  def unapply[Ch]( p: Drop[Ch])
  : Option[(Ch)] = {
    Some(p.x)
  }
}
