
class Rational(x:Int, y:Int) {
  require(y != 0, "the denominter must be non-zero")
  private def gcd(a:Int,b:Int): Int=if (b==0) a else gcd(b,a%b)
  private val g=gcd(x,y)
  def num = x / g
  def denom = y / g
  override def toString=num + "/" + denom
  def + (that: Rational)=
    new Rational(
      num*that.denom+denom*that.num,
      denom*that.denom
    )
  def < (that:Rational)=num*that.denom < denom*that.num
  def max(that:Rational)=if (this < that)  that else this

  def unary_- = new Rational(-num, denom)

  def - (that:Rational)= this + -that

  def * (that:Rational)=
    new Rational(num*that.num,
      denom*that.denom)

  def rec()=new Rational(denom,num)

  def div (that:Rational)= this * that.rec
}

val x =new Rational(3,6)
val y = new Rational(2,3)
x < y
x max y
val z = x.div(y)
x - y - z