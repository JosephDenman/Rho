import scala.collection.immutable.HashMap

case class Zero[Ch]() extends Proc[Ch] {

  def boundNames: HashMap[Ch,Rho]

  def freeNames: HashMap[Ch,Rho]

  override def toString: String = s"0"

  override def equals( o: Any ) : Boolean = {
    o match {
      case that : Zero[Ch] =>
        that.equals(o)
      case _ =>
        false
    }
  }

  override def hashCode( ) : Int = {
    3 * ().hashCode()
  }
}

object Zero {
  def apply[Ch](): Zero[Ch] = {
    new Zero()
  }
  def unapply[Ch]( p: Zero[Ch])
  : Option[()] = {
    None
  }
}