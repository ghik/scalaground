package spanish

import com.avsystem.commons._
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import reactivemongo.bson.Macros.Annotations.Key
import reactivemongo.bson.{BSONDocument, BSONDocumentReader, BSONDocumentWriter, BSONHandler, Macros}

case class WordData(
  @Key("_id") word: String,
  translations: Seq[Translation],
  conjugations: Option[AllConjugations],
  added: JDate,
  seq: Int,
  bucket: Int = 0,
  correctCount: Int = 0,
  incorrectCount: Int = 0,
  lastCorrect: Option[JDate] = None,
  lastIncorrect: Option[JDate] = None
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

case class Conjugation(
  firstSingular: String,
  secondSingular: String,
  thirdSingular: String,
  firstPlural: String,
  secondPlural: String,
  thirdPlural: String
) {
  def toSeq = Seq(firstSingular, secondSingular, thirdSingular, firstPlural, secondPlural, thirdPlural)
}
object Conjugation {
  implicit val writer: BSONDocumentWriter[Conjugation] =
    Macros.writer[Conjugation]
  implicit val reader: BSONDocumentReader[Conjugation] =
    Macros.reader[Conjugation]
}
case class AllConjugations(
  gerund: String,
  participle: String,
  indicativePresent: Conjugation,
  indicativePreterite: Conjugation,
  indicativeImperfect: Conjugation,
  indicativeConditional: Conjugation,
  indicativeFuture: Conjugation,
  subjunctivePresent: Conjugation,
  subjunctiveImperfect: Conjugation,
  subjunctiveImperfect2: Conjugation,
  subjunctiveFuture: Conjugation,
  imperative: Conjugation,
  presentPerfect: Conjugation,
  preteritePerfect: Conjugation,
  pastPerfect: Conjugation,
  conditionalPerfect: Conjugation,
  futurePerfect: Conjugation,
  subjunctivePresentPerfect: Conjugation,
  subjunctivePastPerfect: Conjugation,
  subjunctiveFuturePerfect: Conjugation
)
object AllConjugations {
  implicit val writer: BSONDocumentWriter[AllConjugations] =
    Macros.writer[AllConjugations]
  implicit val reader: BSONDocumentReader[AllConjugations] =
    Macros.reader[AllConjugations]
}


abstract class VerbClass(endings: String*) {
  def unapply(verb: String): Opt[String] =
    Opt(verb).flatMap(v => endings.find(v.endsWith).toOpt).map(verb.stripSuffix)
}

object Ar extends VerbClass("ar")
object Er extends VerbClass("er")
object Ir extends VerbClass("ir", "ír")
object ErIr extends VerbClass("er", "ir", "ír")
object ArErIr extends VerbClass("ar", "er", "ir", "ír")
object Car extends VerbClass("car")
object Cer extends VerbClass("cer")
object Gar extends VerbClass("gar")
object Zar extends VerbClass("zar")

sealed abstract class Person(repr: String) {
  override def toString = repr
}
object Person {
  case object Yo extends Person("yo")
  case object Tu extends Person("tú")
  case object El extends Person("él")
  case object Nosotros extends Person("nosotros")
  case object Vosotros extends Person("vosotros")
  case object Ellos extends Person("ellos")

  val Values = Seq(Yo, Tu, El, Nosotros, Vosotros, Ellos)
}

sealed trait Form
object Form {
  case object Infinitive extends Form
  case object Gerund extends Form
  case object Participle extends Form
  sealed trait PersonalForm extends Form {
    def person: Person
  }
  case class IndicativePresent(person: Person) extends PersonalForm
  case class IndicativePreterite(person: Person) extends PersonalForm
  case class IndicativeImperfect(person: Person) extends PersonalForm
  case class IndicativeConditional(person: Person) extends PersonalForm
  case class IndicativeFuture(person: Person) extends PersonalForm
  case class SubjunctivePresent(person: Person) extends PersonalForm
  case class SubjunctiveImperfect1(person: Person) extends PersonalForm
  case class SubjunctiveImperfect2(person: Person) extends PersonalForm
  case class SubjunctiveFuture(person: Person) extends PersonalForm
  case class Imperative(person: Person) extends PersonalForm
}