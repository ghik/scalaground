import java.lang.reflect.Modifier

import scala.collection.mutable

/**
  * Created by ghik on 08.04.16.
  */
object Reflection {
  def superclasses(cl: Class[_]) =
    Iterator.iterate[Class[_]](cl)(_.getSuperclass).takeWhile(_ != null)

  def allObjectFields(cl: Class[_]) =
    superclasses(cl).flatMap(_.getDeclaredFields.iterator)
      .filterNot(f => Modifier.isStatic(f.getModifiers))
      .filterNot(_.getType.isPrimitive)
      .map({ f => f.setAccessible(true); f })

  def label(any: Any) =
    s"${any.getClass.getName}@${Integer.toHexString(System.identityHashCode(any))}"

  def edges(any: Any) = any match {
    case arr: Array[_] if arr.getClass.getComponentType.isPrimitive => Iterator.empty
    case arr: Array[_] => arr.iterator
    case _ => allObjectFields(any.getClass).map(_.get(any)).filter(_ != null)
  }

  def traverse(root: Any): Unit = {
    val visited = new mutable.HashSet[String]
    val queue = new mutable.Queue[Any]
    queue += root
    visited += label(root)
    while (queue.nonEmpty) {
      val next = queue.dequeue()
      val lab = label(next)
      edges(root).foreach { target =>
        val tlab = label(target)
        if (visited.add(tlab)) {
          queue += target
          println(s"$lab -- $tlab [type=s];")
        }
      }
    }
  }

  def main(args: Array[String]) {
    traverse(Vector.fill(40)("fu"))
  }
}
