
abstract class Intset{
  def include(x: Int): Intset
  def contains(x: Int): Boolean
  def union(x: Intset): Intset
}

class Empty extends Intset{
  def include(x: Int): Intset = new Nonempty(x, new Empty, new Empty)
  def contains(x: Int): Boolean = false
  override def toString = "."
  def union(x: Intset): Intset = x
}

class Nonempty(elem: Int, left: Intset, right: Intset) extends Intset{
  def include(x: Int): Intset =
    if(x < elem) new Nonempty(elem, left include x, right)
    else if(x > elem) new Nonempty(elem, left, right include x )
    else this

  def contains(x: Int): Boolean =
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else true
  override def toString = "{" + left + elem + right + "}"

  def union(x: Intset): Intset =
    ((left union right) union x) include elem
}

val x = new Nonempty(3, new Empty, new Empty)
val y = x.include(5)
val z = new Nonempty(6, new Empty, new Empty)
z union y