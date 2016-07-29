package exercises

import cats._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by ghik on 05.07.16.
  */
class CatsTest extends FunSuite with Matchers {
  test("simple") {
    import cats.implicits._

    Semigroup[Int].combine(1, 2) should be(3)
    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(List(1,2,3,4,5,6))
    Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(Option(3))
    Semigroup[Option[Int]].combine(Option(1), None) should be(Option(1))
    Semigroup[Int => Int].combine({ (x: Int) => x + 1 }, { (x: Int) => x * 10 }).apply(6) should be(67)
  }
}
