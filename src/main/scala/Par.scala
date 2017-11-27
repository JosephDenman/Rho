

case class Par[Ch](left: Proc[Ch],
                   right: Proc[Ch],
                  ) extends Proc[Ch]{

  def boundNames:

  def freeNames:

  override def equals( o : Any ) : Boolean = {
    o match {
      case that : Par[Ch] =>
        left.equals(that.left) && right.equals(that.right)
      case _ =>
        false
    }
  }

  override def hashCode( ) : Int = {
    13 * left.hashCode + 13 * right.hashCode()
  }
}

object Par {
  def apply[Ch](left: Proc[Ch], right: Proc[Ch]): Par[Ch] = {
    new Par(left,right)
  }
  def unapply[Ch](par: Par[Ch])
  : Option[(Proc[Ch], Proc[Ch])] = {
    Some(par.left, par.right)
  }
}