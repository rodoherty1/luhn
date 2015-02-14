package io.rob

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}

/**
 * Created by rob on 11/02/15.
 */
class CreditCardGenerator extends Properties("CreditCard") {
  type CreditCard = List[Int]

  def merge[A](l1: List[A], l2: List[A]): List[A] = {
    def loop(as: List[A], bs: List[A], acc: List[A]): List[A] = {
      (as, bs) match {
        case (Nil, Nil) => acc.reverse
        case ((h1 :: t1), (h2 :: t2)) => loop(t1, t2, h2 :: (h1 :: acc))
        case (Nil, (h2 :: t2)) => loop(Nil, Nil, h2 :: acc)
      }
    }

    loop(l1, l2, List())
  }

  lazy val list1: Gen[CreditCard] = for {
    l1 <- Gen.listOfN(5, Gen.choose(0, 9))

  // Problem ... This line does not do what I want.  It just generates even numbers below twenty.
  // What I need is a set of single digit numbers that, when doubled and whose digits are added together,
  // can be summed with l1 and be divisible by 10.

  // More work to do.
    l2 <- Gen.listOfN(6, Gen.choose(0, 9) map (_ * 2))
  } yield (merge(l1, l2))


  implicit lazy val creditCard: Arbitrary[CreditCard] = Arbitrary(list1)

  property("Validate all properly formed CreditCards") = forAll { (c: CreditCard) =>
    println(c)
    Luhn(c mkString(""))
  }
}
