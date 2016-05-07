import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileWriter, PrintWriter}
import java.net.URL
import java.util.concurrent.Executors
import javax.imageio.ImageIO
import javax.swing._

import com.avsystem.commons._
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import com.google.common.io.ByteStreams
import org.imgscalr.Scalr
import org.imgscalr.Scalr.Mode.{FIT_TO_HEIGHT, FIT_TO_WIDTH}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{MongoDriver, ReadPreference}
import reactivemongo.bson.Macros.Annotations.Key
import reactivemongo.bson._
import upickle.Js

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.{Source, StdIn}
import scala.util.Random

case class WordData(
  @Key("_id") word: String,
  translations: Seq[Translation],
  added: JDate,
  seq: Int
)
object WordData {
  implicit val writer: BSONDocumentWriter[WordData] =
    Macros.writer[WordData]
  implicit val reader: BSONDocumentReader[WordData] =
    Macros.reader[WordData]
}

case class Translation(
  dict: String,
  speechPart: String,
  context: List[String],
  english: List[String],
  example: Option[Example],
  ask: Boolean = false,
  imageUrl: Option[String] = None
) {
  def baseSpeechPart = speechPart.lastIndexOf(' ') match {
    case -1 => speechPart
    case idx => speechPart.substring(idx + 1)
  }
  def articles = speechPart match {
    case "masculine noun" => List("el")
    case "feminine noun" => List("la")
    case "masculine or feminine noun" => List("el", "la")
    case _ => Nil
  }
  def meaningLine = s"From $dict: $speechPart: (${context.mkString(", ")}) ${english.mkString(", ")}"
}
case class Example(spanish: String, english: String)
object Translation {
  implicit val handler: BSONHandler[BSONDocument, Translation] =
    Macros.handler[Translation]
}
object Example {
  implicit val handler: BSONHandler[BSONDocument, Example] =
    Macros.handler[Example]
}

case class ImageData(url: String, data: Array[Byte])

trait Spanish {
  val ParticipleLabel = "Participle: "
  val VowelStemEndings = Set('a', 'o', 'e')

  def participle(verb: String) = {
    val body = Jsoup.connect(s"http://www.spanishdict.com/conjugate/$verb").get.body
    body.getElementsContainingOwnText(ParticipleLabel).get(0).ownText().stripPrefix(ParticipleLabel)
  }

  abstract class VerbClass(ending: String) {
    def unapply(verb: String): Option[String] =
      Some(verb).filter(_.endsWith(ending)).map(_.stripSuffix(ending))
  }

  object Ar extends VerbClass("ar")
  object Er extends VerbClass("er")
  object Ir extends VerbClass("ir")

  def regularParticiple(verb: String) = verb match {
    case Ar(stem) => stem + "ado"
    case Er(stem) if VowelStemEndings.contains(stem.last) => stem + "ído"
    case Er(stem) => stem + "ido"
    case Ir(stem) if VowelStemEndings.contains(stem.last) => stem + "ído"
    case Ir(stem) => stem + "ido"
  }

  def shuffle[T](arr: Array[T]): Unit = {
    val r = new Random
    var i = 0
    while (i < arr.length) {
      val idx = i + r.nextInt(arr.length - i)
      val tmp = arr(i)
      arr(i) = arr(idx)
      arr(idx) = tmp
      i += 1
    }
  }

  val articles = Set("el", "la", "el/la", "los", "las")

  def article(word: String) =
    articles.find(a => word.startsWith(a + " ")).toOpt
  def stripArticle(word: String) =
    article(word).map(a => word.stripPrefix(a + " ")).getOrElse(word)

  def translate(spanish: String) = {
    val doc = Jsoup.connect(s"http://www.spanishdict.com/translate/$spanish").get
    doc.body.getElementById("translate-en").opt.getOrElse(doc.body)
      .getElementsByClass("quicktrans-es").opt.filterNot(_.isEmpty)
      .map(_.first.text.filter(_ <= 127).split(",").map(_.trim).toList)
  }

  def englishMeanings(spanish: String) = {
    def entryTitleElements(entryTitle: Element) =
      Iterator.iterate(entryTitle)(_.nextElementSibling).drop(1)
        .takeWhile(s => s != null && !s.classNames.contains("dictionary-neodict-entry-title"))

    def extractContexts(el: Element) =
      el.children.iterator.asScala.filter(_.classNames.contains("context"))
        .map(_.text.trim.stripPrefix("(").stripSuffix(")")).toList

    def parseDoc(doc: Document) = {
      val englishScope = doc.body.getElementById("translate-en").opt.getOrElse(doc.body)

      def parseEntries(dict: String) =
        englishScope.getElementsByAttributeValue("class", s"dictionary-entry dictionary-$dict").iterator.asScala
          .flatMap(_.getElementsByClass(s"dictionary-$dict-entry-title").iterator.asScala)
          .flatMap { titleElem =>
            val title = titleElem.text.trim.toLowerCase
            entryTitleElements(titleElem)
              .filter(_.classNames.contains("part_of_speech"))
              .flatMap { posEl =>
                val partOfSpeechOpt = posEl.text.trim.toLowerCase match {
                  case "masculine or feminine noun" =>
                    if (title == s"el $spanish, la $spanish") "masculine or feminine noun".opt
                    else if (title == s"el $spanish, la ${spanish}a") "masculine noun".opt
                    else if (title == s"el $spanish, la ${spanish.dropRight(1)}a") "masculine noun".opt
                    else if (title == s"el ${spanish.dropRight(1)}, la $spanish") "feminine noun".opt
                    else if (title == s"el ${spanish.dropRight(1)}e, la $spanish") "feminine noun".opt
                    else if (title == s"el ${spanish.dropRight(1)}o, la $spanish") "feminine noun".opt
                    else if (title == spanish) "masculine or feminine noun".opt
                    else Opt.Empty
                  case partOfSpeech if title == spanish =>
                    partOfSpeech.opt
                  case pos =>
                    Opt.Empty
                }
                partOfSpeechOpt.iterator.flatMap { partOfSpeech =>
                  Iterator.iterate(posEl)(_.nextElementSibling).drop(1)
                    .takeWhile(s => s != null && s.classNames.contains(s"dictionary-$dict-indent-1"))
                    .flatMap { indent1Element =>
                      val context = extractContexts(indent1Element)
                      val translationElements = indent1Element.getElementsByClass(s"dictionary-$dict-indent-2")
                        .iterator.asScala.flatMap(_.getElementsByClass(s"dictionary-$dict-translation").iterator.asScala)
                      translationElements.map { transEl =>
                        val transContext = extractContexts(transEl)
                        val translation = transEl
                          .getElementsByClass(s"dictionary-$dict-translation-translation").first.text.trim
                          .split("(,|;)\\s+").toList.filter(_.nonEmpty)
                        val exampleOpt = transEl.nextElementSibling.option
                          .filter(_.classNames.contains(s"dictionary-$dict-indent-3"))
                          .filterNot(_.children.isEmpty).map(_.child(0))
                          .map { exampleEl =>
                            val spanishExample = exampleEl.child(0).text.trim
                            val englishExample = exampleEl.child(2).text.trim
                            Example(spanishExample, englishExample)
                          }
                        Translation(dict, partOfSpeech, (context ++ transContext).distinct, translation, exampleOpt)
                      }
                    }
                }
              }
          }

      (parseEntries("neodict") ++ parseEntries("neoharrap")).toList
    }

    val first = parseDoc(Jsoup.connect(s"http://www.spanishdict.com/translate/el $spanish").get)
    if (first.nonEmpty) first else parseDoc(Jsoup.connect(s"http://www.spanishdict.com/translate/$spanish").get)
  }

  def writeTo(file: String, content: String): Unit = {
    val pw = new PrintWriter(new FileWriter(file))
    pw.println(content)
    pw.close()
  }

  def imageBytes(bi: BufferedImage): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    ImageIO.write(bi, "png", baos)
    baos.toByteArray
  }

  def imageFromBytes(bytes: Array[Byte]) =
    ImageIO.read(new ByteArrayInputStream(bytes))

  def invokeUI(code: => Any): Unit =
    SwingUtilities.invokeLater(jRunnable(code))
}

trait SpanishMongo extends Spanish {
  val driver = new MongoDriver
  val connection = driver.connection(List("localhost"))
  val db = connection("words")
  val wordsColl: BSONCollection = db("words")
  val unknownWordsColl: BSONCollection = db("unknownWords")
  val imageData: BSONCollection = db("imageData")

  implicit def ec: ExecutionContext = driver.system.dispatcher
  val blockingExecutionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool)

  def loadImageData(url: String) =
    Future(ByteStreams.toByteArray(new URL(url).openStream()))(blockingExecutionContext)
      .filter(bytes => ImageIO.read(new ByteArrayInputStream(bytes)) != null)
      .map(Option(_)).recover({ case _ => None })

  def execute(): Future[_]

  def imageDataFor(url: String): Future[Option[Array[Byte]]] =
    imageData.find(BSONDocument("_id" -> url)).one[BSONDocument].flatMap {
      case Some(bson) => Future.successful(bson.getAs[Array[Byte]]("imageData"))
      case None => for {
        result <- loadImageData(url)
        _ = imageData.insert(BSONDocument("_id" -> url, "imageData" -> result))
      } yield result
    }

  def loadImageUrls(word: String) = {
    val url = s"https://www.google.pl/search?q=$word&tbm=isch"
    val doc = Jsoup.connect(url)
      .header("User-Agent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:45.0) Gecko/20100101 Firefox/45.0").get
    doc.getElementsByClass("rg_meta").iterator.asScala.map(_.ownText).map(upickle.json.read)
      .flatMap {
        case obj: Js.Obj => obj.value.toMap.get("ou")
        case _ => None
      }
      .collect({ case Js.Str(imgurl) => imgurl })
      .toVector
  }

  def getImages(urls: Vector[String], count: Int): Future[Vector[ImageData]] =
    if (count == 0) Future.successful(Vector.empty)
    else urls match {
      case url +: rest =>
        imageDataFor(url).flatMap {
          case Some(data) => getImages(rest, count - 1).map(ImageData(url, data) +: _)
          case None => getImages(rest, count)
        }
      case _ => Future.successful(Vector.empty)
    }

  def main(args: Array[String]): Unit =
    try Await.result(execute(), Duration.Inf) finally {
      Thread.sleep(100)
      connection.close()
      driver.close()
    }

  def chooseImage(imageUrls: Vector[String]) = {
    getImages(imageUrls, 5).foreach { images =>
      invokeUI(ImageChoiceUI.setImages(images.map(_.data)))
    }
    print("choose image: ")
    Some(StdIn.readInt()).filter(i => i > 0 && i <= 5).map(i => imageUrls(i - 1))
  }
}

object ImageChoiceUI extends JFrame {
  val ImageSize = 300
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setLocation(200, 200)

  final class ImageLabel extends JLabel {
    val imageIcon = new ImageIcon
    setIcon(imageIcon)
  }

  val panel = new JPanel
  getContentPane.add(panel)

  def setImages(images: Vector[Array[Byte]]): Unit = {
    val imageCount = images.size
    setSize(ImageSize * imageCount + 100, ImageSize)
    panel.removeAll()
    images.map { bytes => bytes |>
      (new ByteArrayInputStream(_)) |>
      (ImageIO.read(_)) |>
      (i => Scalr.resize(i, if (i.getWidth > i.getHeight) FIT_TO_WIDTH else FIT_TO_HEIGHT, ImageSize, ImageSize)) |>
      (new ImageIcon(_)) |>
      (new JLabel(_))
    }.foreach(panel.add)
    revalidate()
  }
}

object ChooseImages extends SpanishMongo {
  def execute() = {
    invokeUI(ImageChoiceUI.setVisible(true))
    wordsColl.find(BSONDocument("imagesToShow" -> BSONDocument("$exists" -> false)))
      .cursor[BSONDocument](ReadPreference.primary).collect[List]()
      .flatMap { docs =>
        Future.traverse(docs) { doc =>
          val word = doc.getAs[String]("_id").get
          val imageUrls = doc.getAs[Vector[String]]("imageUrls").get
          val translations = doc.getAs[Seq[Translation]]("translations").get
            .map { t =>
              if (t.ask && t.imageUrl.isEmpty) {
                println(s"$word - ${t.meaningLine}")
                t.copy(imageUrl = chooseImage(imageUrls))
              } else t
            }
          wordsColl.update(BSONDocument("_id" -> word),
            BSONDocument("$set" -> BSONDocument("translations" -> translations)))
        }
      }
  }
}

object FixTranslations extends SpanishMongo {
  def execute() = {
    wordsColl.find(BSONDocument("translationsToAsk" -> BSONDocument("$exists" -> true)))
      .cursor[BSONDocument](ReadPreference.primary).collect[List]()
      .flatMap { docs =>
        Future.traverse(docs) { doc =>
          val word = doc.getAs[String]("_id").get
          val translations = doc.getAs[Seq[Translation]]("translations").get
          wordsColl.update(BSONDocument("_id" -> word), BSONDocument(
            "$set" -> BSONDocument("translations" -> translations)
          ))
        }
      }
  }
}

object Inject extends SpanishMongo {
  def execute() = {
    val words = Source.fromFile("/home/ghik/Dropbox/espaniol/palabrasdb.txt")
      .getLines().map(_.trim).filter(_.nonEmpty).map(stripArticle).toSeq.distinct

    def allIds(coll: BSONCollection) =
      coll.find(BSONDocument()).cursor[BSONDocument](ReadPreference.primary).collect[List]()
        .map(_.iterator.map(_.getAs[String]("_id").get).toSet)

    val alreadyPresent = Await.result(for {
      known <- allIds(wordsColl)
      unknown <- allIds(unknownWordsColl)
    } yield known ++ unknown, Duration.Inf)

    val now = new JDate
    val results = words.filterNot(alreadyPresent).zipWithIndex.map { case (word, seq) =>
      val rawTranslations = englishMeanings(word.toLowerCase)
      if (rawTranslations.nonEmpty) {
        println(word)
        println(rawTranslations.iterator.zipWithIndex.map {
          case (trans, i) => s"${i + 1}. ${trans.meaningLine}"
        }.mkString("\n"))
        val indicesToAsk = StdIn.readLine("translations to ask for: ").split(",").iterator
          .map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
        val translations = rawTranslations.zipWithIndex.map { case (t, i) =>
          t.copy(ask = indicesToAsk.contains(i + 1))
        }
        wordsColl.insert(WordData(word, translations, now, seq))
      } else {
        println(s"NO TRANSLATIONS FOR $word FOUND")
        unknownWordsColl.insert(BSONDocument("_id" -> word))
      }
    }

    Future.sequence(results)
  }
}

object Practice extends SpanishMongo {
  case class State(totalAsked: Int = 0, wrong: Vector[(String, Translation)] = Vector.empty) {
    def right = copy(totalAsked = totalAsked + 1)
    def wrong(word: String, question: Translation) =
      copy(totalAsked = totalAsked + 1, wrong = wrong :+(word, question))
  }

  def execute() = {
    print(s"Cuántos palabras recientes vas a practicar? ")
    val count = StdIn.readInt()

    wordsColl.find(BSONDocument()).sort(BSONDocument("added" -> -1, "seq" -> -1))
      .cursor[WordData](ReadPreference.primary)
      .collect[Seq]()
      .map { wordDatas =>
        val translationsByWord = wordDatas.map {
          case WordData(word, allTranslations, _, _) => (word, allTranslations)
        }.toMap
        val rand = new Random
        val questions = wordDatas.iterator.take(count).flatMap {
          case WordData(word, translations, _, _) =>
            translations.filter(_.ask).opt.filter(_.nonEmpty).map(ts => (word, ts(rand.nextInt(ts.size))))
        }.toArray
        shuffle(questions)
        def loop(totalCount: Int, state: State, questions: List[(String, Translation)]): State = {
          def wrongQuestionsToRepeat = {
            val State(total, wrong) = state
            println(s"Has practicando $total palabras, ${wrong.size} incorrecto (${(total - wrong.size) * 100.0 / total}%)")
            val newQuestions = state.wrong.toArray
            shuffle(newQuestions)
            newQuestions.toList
          }

          questions match {
            case (word, qt) :: rest =>
              StdIn.readLine(s"(${state.totalAsked + 1}/$totalCount) ${qt.speechPart}: (${qt.context.mkString(", ")}) ${qt.english.mkString(", ")} ") match {
                case "." => loop(wrongQuestionsToRepeat.size, State(), wrongQuestionsToRepeat)
                case `word` =>
                  println(s"sí")
                  loop(totalCount, state.right, rest)
                case answer if translationsByWord.getOrElse(answer, Nil)
                  .exists(t => t.english == qt.english && t.speechPart == qt.speechPart) =>
                  println(s"sí, pero...")
                  loop(totalCount, state, questions)
                case answer =>
                  println(s"no: $word")
                  loop(totalCount, state.wrong(word, qt), rest)
              }
            case Nil if state.wrong.nonEmpty =>
              loop(wrongQuestionsToRepeat.size, State(), wrongQuestionsToRepeat)
            case _ =>
              state
          }
        }

        loop(questions.length, State(), questions.toList)
      }
  }
}
