package karazin.scala.users.group.week2.homework
import scala.annotation.targetName
import scala.math.{abs, signum}
object Homework:
  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")
    def this(x: Int) = this(x, 1)
    val numer = x / g
    val denom = y / g
    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      (this.numer * that.denom < that.numer * this.denom)
    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      (this < that || this == that)
    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      (!(this <= that))
    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      (!(this < that))

    @targetName("addition")
    infix def +(that: Rational): Rational =
      Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

    @targetName("negation")
    infix def unary_- : Rational =
      Rational(0-this.numer, this.denom)

    @targetName("substraction")
    infix def -(that: Rational): Rational =
      Rational(this.numer * that.denom - that.numer * this.denom, this.denom * that.denom)

    @targetName("multiplication")
    infix def *(that: Rational): Rational =
      Rational(this.numer * that.numer, this.denom * that.denom)

    @targetName("devision")
    infix def /(that: Rational): Rational = {
      require(that.numer != 0, "Denominator is equal to zero")
      val result_numer: Int = this.numer * that.denom
      val result_denom: Int = this.denom * that.numer
      Rational(signum(result_denom)*result_numer, abs(result_denom))
    }

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0
      then a
      else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)
    override def equals(other: Any): Boolean = other match {
      case that: Rational => ((this.numer == that.numer) && (this.denom == that.denom))
      case _ => false
    }
    override def hashCode:
    Int = (7 * this.numer + 31 * this.denom) * 31 + 13

  end Rational

end Homework