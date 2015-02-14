package io.rob

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.prop.Configuration.{MaxDiscarded, PropertyCheckConfig}

/**
 * Created by rob on 11/02/15.
 */
class CreditCardGenerator extends Properties("CreditCard") {
  type CreditCard = List[Int]

  def merge[A](lists: (List[A], List[A])): List[A] = {
    def loop(as: List[A], bs: List[A], acc: List[A]): List[A] = {
      (as, bs) match {
        case (Nil, Nil) => acc
        case ((h1 :: t1), (h2 :: t2)) => loop(t1, t2, h2 :: (h1 :: acc))
        case (Nil, (h2 :: t2)) => loop(Nil, Nil, h2 :: acc)
        case ((h1 :: t1), Nil) => loop(Nil, Nil, h1 :: acc)
      }
    }

    loop(lists._1, lists._2, List())
  }

  def addLists(lists: (List[Int], List[Int])): Int = {
    def doubleAndAddTogether(i: Int): Int = {
      if ((i * 2) < 10) i * 2
      else ((i * 2) % 10) + 1
    }

    lists._1.sum + lists._2.map(doubleAndAddTogether).sum
  }

  def isCreditCard(lists: (List[Int], List[Int])): Boolean = {
    addLists(lists) % 10 == 0
  }

  def isNotCreditCard(lists: (List[Int], List[Int])): Boolean = {
    addLists(lists) % 10 > 0
  }

  lazy val lists: Gen[(List[Int], List[Int])] = for {
    l1 <- Gen.listOfN(6, Gen.choose(0, 9))
    l2 <- Gen.listOfN(5, Gen.choose(0, 9))
  } yield (l1, l2)

  lazy val validCreditCardGenerator: Gen[CreditCard] = for {
    lists <- lists suchThat isCreditCard
  } yield merge(lists)

  lazy val invalidCreditCardGenerator: Gen[CreditCard] = for {
    lists <- lists suchThat isNotCreditCard
  } yield merge(lists)

  implicit lazy val validCreditCard: Arbitrary[CreditCard] = Arbitrary(validCreditCardGenerator)

  implicit lazy val invalidCreditCard: Arbitrary[CreditCard] = Arbitrary(invalidCreditCardGenerator)

  property("Validate all properly formed CreditCards") = forAll(validCreditCardGenerator) { c: CreditCard =>
    Luhn(c mkString "") == true
  }

  property("Validate all illformed CreditCards") = forAll(invalidCreditCardGenerator) { (c: CreditCard) =>
    Luhn(c mkString "") == false
  }

}
