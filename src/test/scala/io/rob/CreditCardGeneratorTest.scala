package io.rob

import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 14/02/15.
 */
class CreditCardGeneratorTest extends WordSpec with Matchers {

  "My CreditCardGenerator" should {
    "merge two lists together" in  {
      val l1 = List(1, 2, 3)
      val l2 = List(4, 5, 6)
      (new CreditCardGenerator).merge(l1, l2) should equal (List(1, 4, 2, 5, 3, 6))
    }
  }


}
