package spanish

import com.avsystem.commons._
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.mongo.{BsonRef, mongoId}
import com.avsystem.commons.serialization.HasGenCodec

case class WrDoc(
  @mongoId word: String,
  doc: String
)
object WrDoc extends HasGenCodec[WrDoc] with BsonRef.Creator[WrDoc]

case class UnknownWord(
  @mongoId word: String
)
object UnknownWord extends HasGenCodec[UnknownWord] with BsonRef.Creator[UnknownWord]

case class WordData(
  @mongoId word: String,
  translations: Seq[Translation],
  conjugations: Option[AllConjugations] = None,
  added: JDate,
  seq: Int,
  bucket: Int = 0,
  correctCount: Int = 0,
  incorrectCount: Int = 0,
  lastCorrect: Option[JDate] = None,
  lastIncorrect: Option[JDate] = None
) {
  val randomizer: Double = math.random
}
object WordData extends HasGenCodec[WordData] with BsonRef.Creator[WordData]

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
object Translation extends HasGenCodec[Translation]
object Example extends HasGenCodec[Example]

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
object Conjugation extends HasGenCodec[Conjugation]
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
object AllConjugations extends HasGenCodec[AllConjugations]


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