package spanish

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileWriter, PrintWriter}
import java.net.{URL, URLEncoder}
import java.util.concurrent.Executors
import javax.imageio.ImageIO
import javax.swing._

import com.avsystem.commons._
import com.avsystem.commons.concurrent.RunInQueueEC
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import com.google.common.io.ByteStreams
import org.imgscalr.Scalr
import org.imgscalr.Scalr.Mode.{FIT_TO_HEIGHT, FIT_TO_WIDTH}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{MongoDriver, ReadPreference}
import reactivemongo.bson._
import upickle.Js

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.{Source, StdIn}
import scala.util.Random

trait RegularConjugations {

  import Form._
  import Person._

  def conjugateRegularly(verb: String, form: Form): String = {
    val baseVerb = verb.stripSuffix("se")
    val baseConjugated = (form, baseVerb) match {
      case (Infinitive, _) => verb
      case (Participle, Ar(stem)) => stem + "ado"
      case (Participle, ErIr(stem)) => stem + "ido"
      case (Gerund, Ar(stem)) => stem + "ando"
      case (Gerund, ErIr(stem)) => stem + "iendo"

      case (IndicativePresent(Yo), Cer(stem)) => stem + "zco"
      case (IndicativePresent(Yo), ArErIr(stem)) => stem + "o"
      case (IndicativePresent(Tu), Ar(stem)) => stem + "as"
      case (IndicativePresent(Tu), ErIr(stem)) => stem + "es"
      case (IndicativePresent(El), Ar(stem)) => stem + "a"
      case (IndicativePresent(El), ErIr(stem)) => stem + "e"
      case (IndicativePresent(Nosotros), Ar(stem)) => stem + "amos"
      case (IndicativePresent(Nosotros), Er(stem)) => stem + "emos"
      case (IndicativePresent(Nosotros), Ir(stem)) => stem + "imos"
      case (IndicativePresent(Vosotros), Ar(stem)) => stem + "áis"
      case (IndicativePresent(Vosotros), Er(stem)) => stem + "éis"
      case (IndicativePresent(Vosotros), Ir(stem)) => stem + "ís"
      case (IndicativePresent(Ellos), Ar(stem)) => stem + "an"
      case (IndicativePresent(Ellos), ErIr(stem)) => stem + "en"

      case (IndicativePreterite(Yo), Car(stem)) => stem + "qué"
      case (IndicativePreterite(Yo), Gar(stem)) => stem + "gué"
      case (IndicativePreterite(Yo), Zar(stem)) => stem + "cé"
      case (IndicativePreterite(Yo), Ar(stem)) => stem + "é"
      case (IndicativePreterite(Yo), ErIr(stem)) => stem + "í"
      case (IndicativePreterite(Tu), Ar(stem)) => stem + "aste"
      case (IndicativePreterite(Tu), ErIr(stem)) => stem + "iste"
      case (IndicativePreterite(El), Ar(stem)) => stem + "ó"
      case (IndicativePreterite(El), ErIr(stem)) => stem + "ió"
      case (IndicativePreterite(Nosotros), Ar(stem)) => stem + "amos"
      case (IndicativePreterite(Nosotros), ErIr(stem)) => stem + "imos"
      case (IndicativePreterite(Vosotros), Ar(stem)) => stem + "asteis"
      case (IndicativePreterite(Vosotros), ErIr(stem)) => stem + "isteis"
      case (IndicativePreterite(Ellos), Ar(stem)) => stem + "aron"
      case (IndicativePreterite(Ellos), ErIr(stem)) => stem + "ieron"

      case (IndicativeImperfect(Yo | El), Ar(stem)) => stem + "aba"
      case (IndicativeImperfect(Yo | El), ErIr(stem)) => stem + "ía"
      case (IndicativeImperfect(Tu), Ar(stem)) => stem + "abas"
      case (IndicativeImperfect(Tu), ErIr(stem)) => stem + "ías"
      case (IndicativeImperfect(Nosotros), Ar(stem)) => stem + "ábamos"
      case (IndicativeImperfect(Nosotros), ErIr(stem)) => stem + "íamos"
      case (IndicativeImperfect(Vosotros), Ar(stem)) => stem + "abais"
      case (IndicativeImperfect(Vosotros), ErIr(stem)) => stem + "íais"
      case (IndicativeImperfect(Ellos), Ar(stem)) => stem + "aban"
      case (IndicativeImperfect(Ellos), ErIr(stem)) => stem + "ían"

      case (IndicativeConditional(Yo | El), v) => v + "ía"
      case (IndicativeConditional(Tu), v) => v + "ías"
      case (IndicativeConditional(Nosotros), v) => v + "íamos"
      case (IndicativeConditional(Vosotros), v) => v + "íais"
      case (IndicativeConditional(Ellos), v) => v + "ían"

      case (IndicativeFuture(Yo), v) => v + "é"
      case (IndicativeFuture(Tu), v) => v + "ás"
      case (IndicativeFuture(El), v) => v + "á"
      case (IndicativeFuture(Nosotros), v) => v + "emos"
      case (IndicativeFuture(Vosotros), v) => v + "éis"
      case (IndicativeFuture(Ellos), v) => v + "án"

      case (SubjunctivePresent(Yo | El), Ar(stem)) => stem + "e"
      case (SubjunctivePresent(Yo | El), ErIr(stem)) => stem + "a"
      case (SubjunctivePresent(Tu), Ar(stem)) => stem + "es"
      case (SubjunctivePresent(Tu), ErIr(stem)) => stem + "as"
      case (SubjunctivePresent(Nosotros), Ar(stem)) => stem + "emos"
      case (SubjunctivePresent(Nosotros), ErIr(stem)) => stem + "amos"
      case (SubjunctivePresent(Vosotros), Ar(stem)) => stem + "éis"
      case (SubjunctivePresent(Vosotros), ErIr(stem)) => stem + "áis"
      case (SubjunctivePresent(Ellos), Ar(stem)) => stem + "en"
      case (SubjunctivePresent(Ellos), ErIr(stem)) => stem + "an"

      case (SubjunctiveImperfect1(Yo | El), Ar(stem)) => stem + "ara"
      case (SubjunctiveImperfect1(Yo | El), ErIr(stem)) => stem + "iera"
      case (SubjunctiveImperfect1(Tu), Ar(stem)) => stem + "aras"
      case (SubjunctiveImperfect1(Tu), ErIr(stem)) => stem + "ieras"
      case (SubjunctiveImperfect1(Nosotros), Ar(stem)) => stem + "áramos"
      case (SubjunctiveImperfect1(Nosotros), ErIr(stem)) => stem + "iéramos"
      case (SubjunctiveImperfect1(Vosotros), Ar(stem)) => stem + "arais"
      case (SubjunctiveImperfect1(Vosotros), ErIr(stem)) => stem + "ierais"
      case (SubjunctiveImperfect1(Ellos), Ar(stem)) => stem + "aran"
      case (SubjunctiveImperfect1(Ellos), ErIr(stem)) => stem + "ieran"

      case (SubjunctiveImperfect2(Yo | El), Ar(stem)) => stem + "ase"
      case (SubjunctiveImperfect2(Yo | El), ErIr(stem)) => stem + "iese"
      case (SubjunctiveImperfect2(Tu), Ar(stem)) => stem + "ases"
      case (SubjunctiveImperfect2(Tu), ErIr(stem)) => stem + "ieses"
      case (SubjunctiveImperfect2(Nosotros), Ar(stem)) => stem + "ásemos"
      case (SubjunctiveImperfect2(Nosotros), ErIr(stem)) => stem + "iésemos"
      case (SubjunctiveImperfect2(Vosotros), Ar(stem)) => stem + "aseis"
      case (SubjunctiveImperfect2(Vosotros), ErIr(stem)) => stem + "ieseis"
      case (SubjunctiveImperfect2(Ellos), Ar(stem)) => stem + "asen"
      case (SubjunctiveImperfect2(Ellos), ErIr(stem)) => stem + "iesen"

      case (Imperative(Tu), v) => conjugateRegularly(v, IndicativePresent(El))
      case (Imperative(El), v) => conjugateRegularly(v, SubjunctivePresent(El))
      case (Imperative(Nosotros), v) => conjugateRegularly(v, SubjunctivePresent(Nosotros))
      case (Imperative(Vosotros), v) => v.stripSuffix("r") + "d"
      case (Imperative(Ellos), v) => conjugateRegularly(v, SubjunctivePresent(Ellos))
    }
    if (verb.endsWith("se")) form match {
      case pf: PersonalForm =>
        val pron = pf.person match {
          case Yo => "me"
          case Tu => "te"
          case El => "se"
          case Nosotros => "nos"
          case Vosotros => "os"
          case Ellos => "se"
        }
        pron + " " + baseConjugated
      case _ => baseConjugated
    }
    else baseConjugated
  }
}

trait Spanish extends RegularConjugations {
  this: SpanishMongo =>
  val ParticipleLabel = "Participle: "
  val VowelStemEndings = Set('a', 'o', 'e')

  implicit class FutureOps[A](fut: Future[A]) {
    def await: A = Await.result(fut, Duration.Inf)
  }

  def participle(verb: String) = {
    val body = Jsoup.connect(s"http://www.spanishdict.com/conjugate/$verb").get.body
    body.getElementsContainingOwnText(ParticipleLabel).get(0).ownText().stripPrefix(ParticipleLabel)
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

  def isEasyTranslation(english: String, spanish: String) =
    english == spanish ||
      english + "o" == spanish ||
      english + "a" == spanish ||
      english.stripPrefix("to ") + "ar" == spanish ||
      english.stripPrefix("to ") + "ir" == spanish ||
      english.stripPrefix("to ") + "er" == spanish ||
      english.stripPrefix("to ").replaceFirst("e$", "ar") == spanish ||
      english.stripPrefix("to ").replaceFirst("e$", "er") == spanish ||
      english.stripPrefix("to ").replaceFirst("e$", "ir") == spanish ||
      english.replaceFirst("e$", "o") == spanish ||
      english.replaceFirst("e$", "a") == spanish ||
      english.replaceFirst("ty$", "dad") == spanish ||
      english.replaceFirst("y$", "ia") == spanish ||
      english.replaceFirst("e$", "ia") == spanish ||
      english.replaceFirst("tion$", "ción") == spanish ||
      english.replaceFirst("sion$", "sión") == spanish ||
      english.replaceFirst("ssion$", "sión") == spanish ||
      english.replaceFirst("te$", "do") == spanish

  def englishMeanings(spanish: String) = {
    def entryTitleElements(entryTitle: Element) =
      Iterator.iterate(entryTitle)(_.nextElementSibling).drop(1)
        .takeWhile(s => s != null && !s.classNames.asScala.exists(_.contains("-entry-title")))

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

                        val fixedPartOfSpeech = partOfSpeech match {
                          case "noun" => wordReferenceSpeechPart(spanish, wordReference(spanish).await) getOrElse "noun"
                          case _ => partOfSpeech
                        }

                        Translation(dict, fixedPartOfSpeech, (context ++ transContext).distinct, translation, exampleOpt)
                      }
                    }
                }
              }
          }

      (parseEntries("neodict") ++ parseEntries("neoharrap")).toList
    }

    val escapedSpanish = URLEncoder.encode(spanish, "UTF-8")
    val variants = ListBuffer(s"el $escapedSpanish", escapedSpanish)
    if (escapedSpanish.endsWith("se")) {
      variants += escapedSpanish.stripSuffix("se")
    }

    variants.toStream
      .map(variant => parseDoc(Jsoup.connect(s"http://www.spanishdict.com/translate/$variant").get))
      .find(_.nonEmpty).getOrElse(Nil)
  }

  def loadConjugations(verb: String): Opt[AllConjugations] = {
    val doc = Jsoup.connect(s"http://www.spanishdict.com/conjugate/${URLEncoder.encode(verb, "UTF-8").stripSuffix("se")}").get

    val conjElem = doc.getElementsByClass("conjugation").first.opt.map { conj =>
      conj.getElementById("conjugate-es").opt.getOrElse(conj)
    }
    conjElem.map { conjugation =>
      val gerund = conjugation.getElementsContainingOwnText("Gerund:").first.text.trim.stripPrefix("Gerund: ")
      val participle = conjugation.getElementsContainingOwnText("Participle:").first.text.trim.stripPrefix("Participle: ")

      def parseCard(card: Element): List[Conjugation] =
        card.getElementsByClass("group").asScala.iterator
          .map(_.getElementsByClass("conj").asScala.iterator.map(_.text.trim).toList).toList
          .map { case List(fs, ss, ts, fp, sp, tp) =>
            if (verb.endsWith("se"))
              Conjugation(s"me $fs", s"te $ss", s"se $ts", s"nos $fp", s"os $sp", s"se $tp")
            else
              Conjugation(fs, ss, ts, fp, sp, tp)
          }

      val List(indicative, subjunctive, imperatives, perfect, perfectSubjunctive, _*) =
        conjugation.getElementsByClass("card").asScala.iterator.drop(1).map(parseCard).toList

      val List(indicativePresent, indicativePreterite, indicativeImperfect, indicativeConditional, indicativeFuture) = indicative
      val List(subjunctivePresent, subjunctiveImperfect, subjunctiveImperfect2, subjunctiveFuture) = subjunctive
      val List(imperative) = imperatives
      val List(presentPerfect, preteritePerfect, pastPerfect, conditionalPerfect, futurePerfect) = perfect
      val List(subjunctivePresentPerfect, subjunctivePastPerfect, subjunctiveFuturePerfect) = perfectSubjunctive

      AllConjugations(
        gerund, participle,
        indicativePresent, indicativePreterite, indicativeImperfect, indicativeConditional, indicativeFuture,
        subjunctivePresent, subjunctiveImperfect, subjunctiveImperfect2, subjunctiveFuture,
        imperative,
        presentPerfect, preteritePerfect, pastPerfect, conditionalPerfect, futurePerfect,
        subjunctivePresentPerfect, subjunctivePastPerfect, subjunctiveFuturePerfect
      )
    }
  }

  def wordReferenceSpeechPart(word: String, doc: Document): Opt[String] = {
    case class WREntry(spanish: Set[String], pos: String, english: Set[String]) {
      def spanishdictPos = pos match {
        case "nm" | "n propio m" => "masculine noun"
        case "nf" | "n propio f" => "feminine noun"
        case "nmf" | "nm, nf" if spanish.size == 2 => "masculine noun"
        case "nmf" | "nm, nf" | "n común" | "n amb" => "masculine or feminine noun"
        case "nfpl" | "nmpl" => "plural noun"
      }
    }

    def extractSpanish(frwrd: Element) =
      frwrd.getElementsByTag("strong").first.text.trim.split(",\\s*").toSet

    def extractPOS(frwrd: Element) =
      frwrd.getElementsByClass("POS2").first.ownText.trim

    def extractEnglish(frwrd: Element): Set[String] = {
      val esen = frwrd.parent
      val evenOdd = if (esen.classNames.contains("even")) "even" else "odd"
      Iterator.iterate(esen)(_.nextElementSibling)
        .takeWhile(e => e != null && e.classNames.contains(evenOdd))
        .flatMap(_.getElementsByClass("ToWrd").iterator.asScala)
        .flatMap(_.ownText.trim.split(",\\s*")).toSet
    }

    val poss = doc.getElementsByClass("FrWrd").iterator.asScala
      .filter(_.parent.attr("id").opt.exists(_.startsWith("esen:")))
      .map(frwrd => WREntry(extractSpanish(frwrd), extractPOS(frwrd), extractEnglish(frwrd)))
      .filter(e => e.pos.startsWith("n") && e.spanish.contains(word))
      .map(_.spanishdictPos).toSet

    if (poss.size == 1) poss.head.opt else Opt.Empty
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
  val wrDocs: BSONCollection = db("wrdocs")

  implicit def ec: ExecutionContext = driver.system.dispatcher
  val blockingExecutionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool)

  def bson(elements: Producer[(String, BSONValue)]*) = BSONDocument(elements: _*)
  def bsonArr(elements: Producer[BSONValue]*) = BSONArray(elements: _*)

  def fetchWords(): Future[Seq[WordData]] = {
    wordsColl.find(bson())
      .cursor[WordData](ReadPreference.primary)
      .collect[Seq]()
  }

  def allVerbData: Future[Seq[WordData]] =
    wordsColl.find(bson("conjugations" -> bson("$exists" -> true)))
      .sort(bson("added" -> -1, "seq" -> -1))
      .cursor[WordData](ReadPreference.primary)
      .collect[Seq]()

  def loadImageData(url: String) =
    Future(ByteStreams.toByteArray(new URL(url).openStream()))(blockingExecutionContext)
      .filter(bytes => ImageIO.read(new ByteArrayInputStream(bytes)) != null)
      .map(Option(_)).recover({ case _ => None })

  def execute(): Future[_]

  def imageDataFor(url: String): Future[Option[Array[Byte]]] =
    imageData.find(bson("_id" -> url)).one[BSONDocument].flatMap {
      case Some(bson) => Future.successful(bson.getAs[Array[Byte]]("imageData"))
      case None => for {
        result <- loadImageData(url)
        _ = imageData.insert(bson("_id" -> url, "imageData" -> result))
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

  def wordReference(word: String): Future[Document] =
    wrDocs.find(bson("_id" -> word)).one[WrDoc].flatMap {
      case Some(WrDoc(_, doc)) => Future.successful(Jsoup.parse(doc))
      case None =>
        val url = s"http://www.wordreference.com/es/en/translation.asp?spen=${URLEncoder.encode(word, "UTF-8")}"
        val doc = Jsoup.connect(url).get()
        val docHtml = doc.outerHtml
        if (docHtml.contains("WordReference is receiving too many requests from your IP address"))
          throw new Exception("overload")
        wrDocs.insert(WrDoc(word, docHtml)).map(_ => doc)
    }

  def main(args: Array[String]): Unit =
    try execute().await finally driver.close()

  def chooseImage(imageUrls: Vector[String]) = {
    getImages(imageUrls, 5).foreach { images =>
      invokeUI(ImageChoiceUI.setImages(images.map(_.data)))
    }
    print("choose image: ")
    Some(StdIn.readInt()).filter(i => i > 0 && i <= 5).map(i => imageUrls(i - 1))
  }
}

object InjectConjugations extends SpanishMongo {
  def execute() = wordsColl.find(bson("conjugations" -> bson("$exists" -> false)))
    .cursor[WordData](ReadPreference.primary).collect[Seq]()
    .flatMap { wordDatas =>
      val verbs = wordDatas.filter(_.translations.exists(_.baseSpeechPart == "verb")).map(_.word)
      Future.traverse(verbs) { verb =>
        Thread.sleep(100)
        println(s"Loading conjugations for $verb")
        val conjugations = loadConjugations(verb).toOption
        wordsColl.update(bson("_id" -> verb), bson("$set" -> bson("conjugations" -> conjugations)))
      }
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
    images.map { bytes =>
      bytes |>
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
    wordsColl.find(bson("imagesToShow" -> bson("$exists" -> false)))
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
          wordsColl.update(bson("_id" -> word),
            bson("$set" -> bson("translations" -> translations)))
        }
      }
  }
}

object FixTranslations extends SpanishMongo {
  def execute() = {
    wordsColl.find(bson("translationsToAsk" -> bson("$exists" -> true)))
      .cursor[BSONDocument](ReadPreference.primary).collect[List]()
      .flatMap { docs =>
        Future.traverse(docs) { doc =>
          val word = doc.getAs[String]("_id").get
          val translations = doc.getAs[Seq[Translation]]("translations").get
          wordsColl.update(bson("_id" -> word), bson(
            "$set" -> bson("translations" -> translations)
          ))
        }
      }
  }
}

object Inject extends SpanishMongo {
  def execute() = {
    val words = Source.fromFile("/home/ghik/Dropbox/espaniol/palabrasdb.txt")
      .getLines().map(_.trim).filter(_.nonEmpty).filterNot(_.startsWith("-")).map(stripArticle)
      .toSeq.distinct

    def allIds(coll: BSONCollection) =
      coll.find(bson()).cursor[BSONDocument](ReadPreference.primary).collect[List]()
        .map(_.iterator.map(_.getAs[String]("_id").get).toSet)

    val alreadyPresent = {
      for {
        known <- allIds(wordsColl)
        unknown <- allIds(unknownWordsColl)
      } yield known ++ unknown
    }.await

    val now = new JDate
    val wordsToInsert = words.iterator
      .filter(w => w.startsWith("+") || !alreadyPresent.contains(w)).map(_.stripPrefix("+")).toVector
    val results = wordsToInsert.iterator.zipWithIndex.map { case (word, seq) =>
      val rawTranslations = englishMeanings(word.toLowerCase)
      if (rawTranslations.nonEmpty) {
        println(s"${seq + 1}/${wordsToInsert.size} ${word.magenta}")
        println(rawTranslations.iterator.zipWithIndex.map {
          case (trans, i) =>
            s"${i + 1}. ${trans.meaningLine}" +
              trans.example.map(e => s"\n   ${e.english}\n   ${e.spanish}").getOrElse("")
        }.mkString("\n"))
        val indicesToAsk = StdIn.readLine("translations to ask for: ").split(",").iterator
          .map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
        if (indicesToAsk.nonEmpty) {
          val translations = rawTranslations.zipWithIndex.map { case (t, i) =>
            t.copy(ask = indicesToAsk.contains(i + 1))
          }
          val conjugations =
            if (translations.exists(_.baseSpeechPart == "verb"))
              loadConjugations(word).toOption
            else None
          wordsColl.update(bson("_id" -> word), WordData(word, translations, conjugations, now, seq), upsert = true)
        } else {
          unknownWordsColl.insert(bson("_id" -> word))
        }
      } else {
        println(s"NO TRANSLATIONS FOR $word FOUND")
        unknownWordsColl.insert(bson("_id" -> word))
      }
    }

    Future.sequence(results)
  }
}

object FixPartOfSpeech extends SpanishMongo {
  def execute() = for {
    allWords <- wordsColl.find(bson()).cursor[WordData](ReadPreference.primary).collect[List]()
    updates <- Future.traverse(allWords)({ wd =>
      val newTrans = wd.translations.map {
        case t if t.speechPart == "noun" =>
          t.copy(speechPart = wordReferenceSpeechPart(wd.word, wordReference(wd.word).await) getOrElse "noun")
        case t => t
      }
      if (newTrans != wd.translations) {
        println(s"New translations for ${wd.word}:${newTrans.mkString("\n", "\n", "\n")}")
        wordsColl.update(bson("_id" -> wd.word), bson("$set" -> bson("translations" -> newTrans)))
      }
      else Future.successful(())
    })(implicitly, RunInQueueEC)
  } yield ()
}

object ParsingTest {
  def main(args: Array[String]): Unit = {

  }
}
