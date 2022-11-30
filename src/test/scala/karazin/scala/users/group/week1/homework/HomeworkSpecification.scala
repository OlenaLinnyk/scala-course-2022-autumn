package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework") :

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)
  include(KolakoskiSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators") :

  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    and(left, right) == (left && right)
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    or(left, right) == (left || right)
  }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers") :

  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    fermatNumber(2) == Math.pow(2, Math.pow(2, 2)) + 1
    fermatNumber(3) == Math.pow(2, Math.pow(2, 3)) + 1
    fermatNumber(5) == Math.pow(2, Math.pow(2, 5)) + 1
  }

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence") :

  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  property("lookAndSaySequenceElement") = forAll { (n: Int) =>
    lookAndSaySequenceElement(1) == 1
    lookAndSaySequenceElement(2) == 11
    lookAndSaySequenceElement(3) == 21
    lookAndSaySequenceElement(4) == 1211
    lookAndSaySequenceElement(5) == 111221
    lookAndSaySequenceElement(6) == 312211
    lookAndSaySequenceElement(7) == 13112221
  }

end LookAndAaSequenceSpecification

object KolakoskiSequenceSpecification extends Properties("KolakoskiSequence") :

  import `KolakoskiSequence`._
  import arbitraries.given Arbitrary[Int]

  property("kolakoskiSequenceElement") = forAll { (n: Int) =>
    kolakoskiSequenceElement(1) == "1"
    kolakoskiSequenceElement(2) == "12"
    kolakoskiSequenceElement(3) == "122"
    kolakoskiSequenceElement(4) == "1221"
    kolakoskiSequenceElement(5) == "12211"
    kolakoskiSequenceElement(6) == "122112"
    kolakoskiSequenceElement(7) == "1221121"
    kolakoskiSequenceElement(8) == "12211212"
    kolakoskiSequenceElement(9) == "122112122"
    kolakoskiSequenceElement(10) == "1221121221"
  }

end KolakoskiSequenceSpecification

