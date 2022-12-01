package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec

/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework:

  object `Boolean Operators`:

    def not(b: Boolean): Boolean =
      if (b == true)
      then false
      else true

    def and(left: Boolean, right: => Boolean): Boolean =
      if (left == true)
      then right
      else false

    def or(left: Boolean, right: => Boolean): Boolean =
      if (left == false)
      then right
      else true

    /*def andExtra(left: Boolean, right: => Boolean): Boolean =
      if (left == true)
      then right
      else false

    def orExtra(left: Boolean, right: => Boolean): Boolean =
      if (left == false)
      then right
      else true*/

  end `Boolean Operators`

  object `Fermat Numbers`:

    val mult: (BigInt, BigInt, BigInt) => BigInt = (x, y, res) =>
      if ((x <= 0) || (y <= 0))
      then res
      else mult(x - 1, y, res + y)

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) => mult(a, b, 0)

    val pow: (BigInt, BigInt, BigInt) => BigInt = (aa, n, result) =>
      if (n == 0)
      then result
      else pow(aa, n - 1, multiplication(aa, result))

    val power: (BigInt, BigInt) => BigInt = (a, b) => pow(a, b, 1)

    val fermatNumber: BigInt => BigInt = n => (power(2, power(2, n)) + 1)

  end `Fermat Numbers`

  object `Look-and-say Sequence`:
    def xnext(res: BigInt, x: BigInt): BigInt =
      if (x == 0)
      then res
      else {
        if ((x % 10) != ((x / 10) % 10))
        then {
          xnext((res * 10 + (x % 10)) * 10 + 1, x / 10)
        }
        else if (((x / 100) % 10) != ((x / 10) % 10))
        then {
          xnext((res * 10 + (x % 10)) * 10 + 2, x / 100)
        }
        else {
          xnext((res * 10 + (x % 10)) * 10 + 3, x / 1000)
        }
      }

    def reverse(res: BigInt, a: BigInt): BigInt =
      if (a == 0)
      then res
      else reverse(res * 10 + (a % 10), a / 10)

    def lookAndSaySequenceEl(n: BigInt, x: BigInt): BigInt =
      if (n == 0)
      then x
      else lookAndSaySequenceEl(n - 1, reverse(0, xnext(0, x)))

    def lookAndSaySequenceElement(n: BigInt): BigInt = lookAndSaySequenceEl(n - 1, 1)

  end `Look-and-say Sequence`

  object `KolakoskiSequence`:

    def kolakoskiSequenceEl(n: Int, s: String, res: String, i: Int, j: Int): String =
      if (n == 0)
      then res
      else {
        if (s(i) == '1')
        then {
          if (s(j) == '2')
          then kolakoskiSequenceEl(n - 1, s + "1", res + s(i), i + 1, j + 1)
          else kolakoskiSequenceEl(n - 1, s + "2", res + s(i), i + 1, j + 1)
        }
        else {
          if (s(j) == '2')
          then kolakoskiSequenceEl(n - 1, s + "11", res + s(i), i + 1, j + 2)
          else kolakoskiSequenceEl(n - 1, s + "22", res + s(i), i + 1, j + 2)
        }
      }

    def kolakoskiSequenceElement(n: Int): String =
      if (n == 1)
      then "1"
      else
      {
        if (n == 2)
        then "12"
        else
        {
          if (n == 3)
          then "122"
          else kolakoskiSequenceEl(n - 2, "122", "12", 2, 2)
        }
      }

  end `KolakoskiSequence`


end Homework