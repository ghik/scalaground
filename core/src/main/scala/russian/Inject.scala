package russian

import com.avsystem.commons._
import com.avsystem.commons.misc.Timestamp
import com.avsystem.commons.mongo.typed.{MongoDataCompanion, MongoEntity, MongoEntityCompanion, TypedMongoCollection}
import com.avsystem.commons.serialization.HasGenCodec
import com.mongodb.reactivestreams.client.{MongoClient, MongoClients, MongoDatabase}
import monix.execution.Scheduler.Implicits.global
import org.bson.types.ObjectId
import org.jsoup.Jsoup

import java.net.URLEncoder
import scala.io.Source

case class PracticeState(
  bucket: Int = 0,
  correctCount: Int = 0,
  incorrectCount: Int = 0,
  lastCorrect: Opt[Timestamp] = Opt.Empty,
  lastIncorrect: Opt[Timestamp] = Opt.Empty
)
object PracticeState extends MongoDataCompanion[PracticeState]

case class PonsWord(
  id: ObjectId,
  word: String,
  noaccent: String,
  added: Timestamp,
  wordclass: Opt[String],
  flexion: Opt[String],
  genus: Opt[String],
  translations: List[PonsTranslation],
  practice: PracticeState
) extends MongoEntity[ObjectId]
object PonsWord extends MongoEntityCompanion[PonsWord]

case class PonsTranslation(
  source: String,
  target: String
)
object PonsTranslation extends HasGenCodec[PonsTranslation]

object Inject {
  final val CombiningAcuteAccent = "\u0301"
  val mongoClient: MongoClient = MongoClients.create()
  val db: MongoDatabase = mongoClient.getDatabase("ruswords")
  val coll: TypedMongoCollection[PonsWord] =
    new TypedMongoCollection[PonsWord](db.getCollection("words"))

  def main(args: Array[String]): Unit = {
    val baseUrl = "https://pl.pons.com/t%C5%82umaczenie/rosyjski-polski/"
    val wordsfile = "C:\\Users\\rjhl9\\Dropbox\\russki\\wyrazy.txt"
    Source.fromFile(wordsfile).getLines().map(_.trim).filter(_.nonEmpty).foreach { searchWord =>
      println(s"Searching $searchWord")
      val doc = Jsoup.connect(s"$baseUrl${URLEncoder.encode(searchWord, "utf-8")}").get()
      val matchingWords = for {
        results <- doc.body.getElementsByClass("results").iterator.asScala
        entry <- results.getElementsByClass("entry").iterator.asScala
        accWord = entry.attr("rel")
        noaccWord = accWord.replace(CombiningAcuteAccent, "")
        if noaccWord == searchWord
        rom <- entry.getElementsByClass("rom").iterator.asScala
      } yield {
        val romhead = rom.getElementsByClass("romhead").first
        val wordclass = romhead.getElementsByClass("wordclass").first.opt.map(_.text)
        val flexion = romhead.getElementsByClass("flexion").first.opt.map(_.text.stripPrefix("<").stripSuffix(">"))
        val genus = romhead.getElementsByClass("genus").first.opt.map(_.text)
        val translations = for {
          transEl <- rom.getElementsByClass("translations").asScala.iterator
          transDl <- transEl.children.asScala.iterator.filter(_.tagName == "dl")
        } yield {
          val source = transDl.getElementsByClass("source").first.text
          val target = transDl.getElementsByClass("target").first.text
          PonsTranslation(source, target)
        }
        PonsWord(new ObjectId,
          accWord, noaccWord, Timestamp.now(), wordclass, flexion, genus,
          translations.toList, PracticeState()
        )
      }

      if (matchingWords.nonEmpty) matchingWords.foreach { pword =>
        println(s"Inserting $pword")
        coll.insertOne(pword).runSyncUnsafe()
        Thread.sleep(500)
      } else {
        println(s"Nothing found for $searchWord")
      }
    }
  }
}
