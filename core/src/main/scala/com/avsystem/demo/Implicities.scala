package com.avsystem.demo

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait StatisticsService
trait UserService
trait EventService

trait SubPanel
class StatisticsPanel(implicit statService: StatisticsService, eventService: EventService)
  extends SubPanel
class UserInfoPanel(implicit userService: UserService)
  extends SubPanel

class UserDashboardPanel(implicit
  statService: StatisticsService,
  userService: UserService,
  eventService: EventService) {

  def addSubpanel(subPanel: SubPanel): Unit = ???

  addSubpanel(new StatisticsPanel)
  addSubpanel(new UserInfoPanel)
}

object simpleTypeClass {

  trait HasDefaultValue[T] {
    def defaultValue: T
  }

  object HasDefaultValue {
    implicit val DefaultInt: HasDefaultValue[Int] =
      new HasDefaultValue[Int] {
        def defaultValue = 42
      }
  }

  def getOrDefault[T](option: Option[T])(implicit hdv: HasDefaultValue[T]): T =
    option.getOrElse(hdv.defaultValue)

  def getOrDefault2[T: HasDefaultValue](option: Option[T]): T =
    option.getOrElse(implicitly[HasDefaultValue[T]].defaultValue)

  getOrDefault(Option.empty[Int])
}

object implicitPriorities {
  trait A
  trait B extends A

  def takeA(implicit a: A): Unit = ???

  implicit val a: A = ???
  //implicit val a2: A = ???

  takeA
}

object restrainedMethod {
  trait Collection[+A] {
    def iterator: Iterator[A]

    def trimJoin(separator: String, length: Int)(implicit ev: A <:< CharSequence): String = {
      val sb = new java.lang.StringBuilder
      iterator.foreach(a => sb.append(a, 0, length))
      sb.toString
    }
  }

  object CollectionTest {
    val coll: Collection[Int] = ???
    //coll.trimJoin(",", 5)

    val strColl: Collection[String] = ???
    strColl.trimJoin(",", 5)
  }

}

object extensionMethods {

  implicit class StringOptionOps(private val opt: Option[String])
    extends AnyVal {

    def orEmpty = opt.getOrElse("")
  }

  Some("lol").orEmpty

  trait Collection[+A] {
    def iterator: Iterator[A]
  }

  object Collection {

    implicit class CharSequenceCollOps(private val coll: Collection[CharSequence])
      extends AnyVal {

      def join(separator: String): String = {
        val sb = new java.lang.StringBuilder
        coll.iterator.foreach(sb.append)
        sb.toString
      }
    }
  }

  object CollectionTest {
    val coll: Collection[Int] = ???
    //coll.join(",")

    val strColl: Collection[String] = ???
    strColl.join(",")
  }

}

object t3 {

  trait SomeService
  object SomeService {
    implicit val DefaultService: SomeService = new SomeService {}
  }

  def weźDużoListParametrów(str: String, i: Int)
    (implicit ec: ExecutionContext, ss: SomeService): Unit = ???

  implicit val ec: ExecutionContext = ???

  weźDużoListParametrów("lol", 42)

  def extractInts(list: List[Any]): List[Int] =
    list.collect {
      case i: Int => i
    }

  def extract[T](list: List[Any])(implicit ct: ClassTag[T]): List[T] =
    list.collect {
      case t: T => t
    }

  def extractCb[T: ClassTag](list: List[Any]): List[T] =
    list.collect {
      case t: T => t
    }

}

object t4 {

  trait Ordering[T] {
    def compare(x: T, y: T): Int
  }

  trait List[+A] {
    def sortBy[B](f: A => B)(implicit bo: Ordering[B]): List[A]
  }

}

object t5 {

  object Ordering {

    implicit def listOrdering[T: Ordering]: Ordering[List[T]] =
      new Ordering[List[T]] {
        def compare(x: List[T], y: List[T]): Int = (x, y) match {
          case (Nil, Nil) => 0
          case (Nil, _) => -1
          case (_, Nil) => 1
          case (xhead :: xtail, yhead :: ytail) =>
            implicitly[Ordering[T]].compare(xhead, yhead) match {
              case 0 => compare(xtail, ytail)
              case res => res
            }
        }
      }

  }

}

object t6 {

  trait Monoid[T] {
    def zero: T
    def plus(t1: T, t2: T): T
  }

  object Monoid {
    val IntMonoid: Monoid[Int] = new Monoid[Int] {
      def zero: Int = 0
      def plus(t1: Int, t2: Int): Int = t1 + t2
    }
    val StringMonoid: Monoid[String] = new Monoid[String] {
      def zero: String = ""
      def plus(t1: String, t2: String): String = t1 + t2
    }
  }

}

object t7 {
  trait ByteString
  object ByteString {
    implicit class ops(private val bs: ByteString) extends AnyVal {
      def getString = bs.toString
    }
  }

  def takeBytes(bytes: ByteString): Unit = ???

  def fromStringUtf8(str: String): ByteString = ???

  implicit class stringOps(private val str: String) extends AnyVal {
    def bytes = fromStringUtf8(str)
  }


  def method(implicit param: Ordering[ByteString]): Unit = ???

  takeBytes("jksldjfklsdjls".toLong.toString.bytes)
}

object Implicities {

  def getUserIdByName(name: String): Future[Long] = ???
  def getUserAge(id: Long): Future[Int] = ???

  def getMessage(username: String)(implicit ec: ExecutionContext): Future[String] = {
    getUserIdByName(username)
      .flatMap(id => getUserAge(id)
        .map(age => s"User $username (ID $id) is $age years old")
      )
  }

  def getMessage2(username: String)(implicit ec: ExecutionContext): Future[String] =
    for {
      id <- getUserIdByName(username)
      age <- getUserAge(id)
    } yield s"User $username (ID $id) is $age years old"

}
