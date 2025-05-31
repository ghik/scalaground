package spanish

import com.avsystem.commons._
import com.avsystem.commons.misc.{Opt, Timestamp}
import com.avsystem.commons.mongo.typed.{MongoDataCompanion, MongoEntity, MongoEntityCompanion}

case class WrDoc(
  id: String,
  doc: String
) extends MongoEntity[String]
object WrDoc extends MongoEntityCompanion[WrDoc]

case class UnknownWord(
  id: String
) extends MongoEntity[String]
object UnknownWord extends MongoEntityCompanion[UnknownWord]

case class WordData(
  id: String,
  categories: Seq[String] = Nil,
  translations: Seq[Translation],
  conjugations: Option[AllConjugations] = None,
  added: Timestamp,
  seq: Int,
  bucket: Int = 0,
  correctCount: Int = 0,
  incorrectCount: Int = 0,
  lastCorrect: Option[Timestamp] = None,
  lastIncorrect: Option[Timestamp] = None
) extends MongoEntity[String] {
  val randomizer: Double = math.random()
}
object WordData extends MongoEntityCompanion[WordData]

case class Translation(
  dict: String,
  speechPart: String,
  context: List[String],
  english: List[String],
  example: Option[Example] = None,
  ask: Boolean = false,
  imageUrl: Option[String] = None
) {
  def baseSpeechPart: String = speechPart.lastIndexOf(' ') match {
    case -1 => speechPart
    case idx => speechPart.substring(idx + 1)
  }
  def articles: List[String] = speechPart match {
    case "masculine noun" => List("el")
    case "feminine noun" => List("la")
    case "masculine or feminine noun" => List("el", "la")
    case _ => Nil
  }
  def meaningLine: String = {
    val meanings = english.map(e => if (dict == "neodict") e.yellow else e.blue).mkString(", ")
    s"From $dict: $speechPart: (${context.mkString(", ")}) $meanings"
  }
}

case class Example(spanish: String, english: String)
object Translation extends MongoDataCompanion[Translation]
object Example extends MongoDataCompanion[Example]

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
object Conjugation extends MongoDataCompanion[Conjugation]
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
object AllConjugations extends MongoDataCompanion[AllConjugations]

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
  override def toString: String = repr
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
