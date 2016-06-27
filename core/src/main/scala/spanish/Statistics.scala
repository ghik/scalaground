package spanish

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDateTime, ZoneId}

import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import reactivemongo.api.ReadPreference
import reactivemongo.bson.BSONDocument

import scala.util.Success

/**
  * Created by ghik on 05.06.16.
  */
object Statistics extends SpanishMongo {
  def toLocalDT(date: JDate) =
    LocalDateTime.ofInstant(Instant.ofEpochMilli(date.getTime), ZoneId.systemDefault())

  def execute() = {
    wordsColl.find(BSONDocument())
      .sort(BSONDocument("lastCorrect" -> 1))
      .cursor[WordData](ReadPreference.primary)
      .collect[Seq]()
      .andThen { case Success(wordDatas) =>
        def counted[T](it: Iterator[T], current: Opt[(T, Int)] = Opt.Empty): Iterator[(T, Int)] =
          if (it.hasNext) {
            val t = it.next()
            current match {
              case Opt((`t`, count)) => counted(it, Opt((t, count + 1)))
              case Opt(prev) => Iterator(prev) ++ counted(it)
              case Opt.Empty => counted(it, Opt((t, 1)))
            }
          } else current.toIterator

        val countedList = counted(wordDatas.iterator
          .map(wd => toLocalDT(wd.lastCorrect.getOrElse(new JDate(0))).truncatedTo(ChronoUnit.DAYS)))
          .toList

        countedList.foreach(println)
      }
  }
}
