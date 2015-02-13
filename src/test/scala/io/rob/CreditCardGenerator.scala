package io.rob

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}

/**
 * Created by rob on 11/02/15.
 */
class CreditCardGenerator extends Properties("CreditCard"){
  type CreditCard = List[Int]

  def isCreditCard(l1: List[Int], l2: List[Int]) = {
    val l3 = l2 map (_ * 2)
    (l1.sum + l3.sum) % 10 == 0
  }

  def zip(l1: List[Int], l2: List[Int]): List[Int] = {
    def loop (ll1: List[Int], ll2: List[Int], zipped: List[Int]): List[Int] = {
      (ll1, ll2) match {
        case (Nil, Nil) => zipped
        case ((h1 :: t1),(h2 :: t2)) => loop (t1, t2, h1 :: h2 :: zipped)
      }
    }

  }

  lazy val list1: Gen[CreditCard] = for {
    l1 <- Gen.listOfN(5, arbitrary[Int])
    l2 <- Gen.listOfN(6, arbitrary[Int])
    if isCreditCard(l1, l2)
  } yield zip(l1, l2)

  implicit lazy val creditCard: Arbitrary[CreditCard] = Arbitrary(list1)

  property("prop1") = forAll { (c: CreditCard) =>
    Luhn(c.mkString(""))
  }



}
