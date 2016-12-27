package spanish

import com.avsystem.commons._
import com.avsystem.commons.jiop.JavaInterop._
import org.apache.commons.lang3.time.DateUtils
import reactivemongo.bson.BSONDocument
import spanish.Form._

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Random

trait Practice extends SpanishMongo {
  case class State[T](totalAsked: Int = 0, wrong: Vector[(String, T)] = Vector.empty) {
    def right: State[T] = copy(totalAsked = totalAsked + 1)
    def wrong(word: String, question: T): State[T] =
      copy(totalAsked = totalAsked + 1, wrong = wrong :+ (word, question))
  }
}

object PracticeConjugation extends Practice {
  def execute() = {
    print(s"Cuántos verbos recientes vas a practicar? ")
    val count = StdIn.readInt()

    allVerbData.map { verbDatas =>
      val rand = new Random
      val questions = verbDatas.iterator.take(count).flatMap { wd =>
        val verb = wd.word
        val conjs = wd.conjugations.get
        def mkQuestions(conj: Conjugation, form: Person => Form, hint: String) =
          (conj.toSeq.map(_.split(",").toSeq) zip Person.Values).filter {
            case (conjugated, person) => conjugated != Seq(conjugateRegularly(verb, form(person)))
          } map {
            case (conjugated, person) => (s"$person$hint ($verb)", conjugated)
          }

        val gerundQuestion = Seq(conjs.gerund.split(",").toSeq)
          .filter(_ != Seq(conjugateRegularly(verb, Gerund))).map(g => (s"estoy ($verb)", g))
        val participleQuestion = Seq(conjs.participle.split(",").toSeq)
          .filter(_ != Seq(conjugateRegularly(verb, Participle))).map(g => (s"yo he ($verb)", g))
        val presentQuestions = mkQuestions(conjs.indicativePresent, IndicativePresent, "")
        val pastQuestions = mkQuestions(conjs.indicativePreterite, IndicativePreterite, " ayer")
        val futureQuestions = mkQuestions(conjs.indicativeFuture, IndicativeFuture, " mañana")
        val subjQuestion = mkQuestions(conjs.subjunctivePresent, SubjunctivePresent, "")
        val imperfectQuestions = mkQuestions(conjs.indicativeImperfect, IndicativeImperfect, " cada día")
        val allQuestions = gerundQuestion ++ participleQuestion ++ presentQuestions ++ pastQuestions ++ imperfectQuestions ++ futureQuestions

        Some(allQuestions).filter(_.nonEmpty).map(aq => aq(rand.nextInt(aq.size)))
      }.toArray
      shuffle(questions)
      def loop(totalCount: Int, state: State[Seq[String]], questions: List[(String, Seq[String])]): State[Seq[String]] = {
        def wrongQuestionsToRepeat = {
          val State(total, wrong) = state
          println(s"Has practicando $total palabras, ${wrong.size} incorrectas (${(total - wrong.size) * 100.0 / total}%)")
          val newQuestions = state.wrong.toArray
          shuffle(newQuestions)
          newQuestions.toList
        }

        questions match {
          case (question, expectedAnswers) :: rest =>
            StdIn.readLine(s"(${state.totalAsked + 1}/$totalCount): $question ") match {
              case "." => loop(wrongQuestionsToRepeat.size, State(), wrongQuestionsToRepeat)
              case answer if expectedAnswers.contains(answer) =>
                val also =
                  if (expectedAnswers.size > 1)
                    s", también: ${expectedAnswers.filter(_ != answer).mkString(", ")}"
                  else ""
                println(s"sí$also")
                loop(totalCount, state.right, rest)
              case answer =>
                println(s"${"NO".red}: ${expectedAnswers.mkString(", ")}")
                loop(totalCount, state.wrong(question, expectedAnswers), rest)
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

object PracticeArticles extends Practice {
  def execute() = {
    def isIrregularNoun(word: String, trans: Translation) = trans.speechPart match {
      case "masculine noun" => !word.endsWith("o")
      case "feminine noun" => !(word.endsWith("a") || word.endsWith("ad") || word.endsWith("ción") || word.endsWith("sión"))
      case "masculine or feminine noun" => true
      case _ => false
    }

    fetchWords().map { wordDatas =>
      val article: PartialFunction[String, String] = {
        case "masculine noun" => "el"
        case "feminine noun" => "la"
        case "masculine or feminine noun" => "el/la"
      }

      val translationsByWord = wordDatas.map { wd =>
        wd.translations.filter(t => article.isDefinedAt(t.speechPart))
          .groupBy(t => s"${article(t.speechPart)} ${wd.word}")
      }.reduce(_ ++ _)

      val questions = wordDatas.flatMap { wd =>
        wd.translations.filter(_.ask).map(t => (wd.word, t)).filter((isIrregularNoun _).tupled)
      }.toArray
      shuffle(questions)

      def loop(totalCount: Int, state: State[Translation], questions: List[(String, Translation)]): State[Translation] = {
        def wrongQuestionsToRepeat = {
          val State(total, wrong) = state
          println(s"Has practicando $total palabras, ${wrong.size} incorrectas (${(total - wrong.size) * 100.0 / total}%)")
          val newQuestions = state.wrong.toArray
          shuffle(newQuestions)
          newQuestions.toList
        }

        questions match {
          case (word, qt) :: rest =>
            val expectedAnswer = s"${article(qt.speechPart)} $word"
            StdIn.readLine(s"(${state.totalAsked + 1}/$totalCount): (${qt.context.mkString(", ")}) ${qt.english.mkString(", ")} ") match {
              case "." => loop(wrongQuestionsToRepeat.size, State(), wrongQuestionsToRepeat)
              case `expectedAnswer` =>
                println(s"sí")
                loop(totalCount, state.right, rest)
              case answer if translationsByWord.getOrElse(answer, Nil).exists(t => t.english == qt.english) =>
                println(s"sí, pero...")
                loop(totalCount, state, questions)
              case answer =>
                println(s"${"NO".red}: $expectedAnswer")
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

object PracticeWords extends Practice {
  val minIntervalHours = 2
  val bucketChange = 1

  case class ChosenTranslation(wd: WordData, translation: Translation)

  def execute() = {
    print(s"Cuántos palabras vas a practicar? ")
    val count = StdIn.readInt()

    fetchWords().map(_.sortBy(wd => (wd.bucket, wd.lastCorrect))).map { wordDatas =>
      val translationsByWord = wordDatas.iterator.map(wd => (wd.word, wd.translations)).toMap
      val rand = new Random
      val maxLastCorrect = DateUtils.addHours(new JDate, -minIntervalHours)
      val questions = wordDatas.iterator.filter {
        wd => wd.lastCorrect.forall(_.before(maxLastCorrect))
      }.flatMap { wd =>
        val word = wd.word
        val translations = wd.translations
        // t.english.exists(isEasyTranslation(_, word))
        translations.filter(t => t.ask && t.english.nonEmpty)
          .opt.filter(_.nonEmpty)
          .map(ts => (word, ChosenTranslation(wd, ts(rand.nextInt(ts.size)))))
      }.take(count).toArray
      shuffle(questions)

      val bucketStats = questions.iterator.map(_._2.wd.bucket)
        .foldLeft(ArrayBuffer.fill(5)(0)) {
          case (acc, b) => acc(b) += 1; acc
        }
      println(s"Bucket statistics: ${bucketStats.mkString(",")}")

      def loop(
        round: Int,
        totalCount: Int,
        state: State[ChosenTranslation],
        questions: List[(String, ChosenTranslation)],
        repeated: Boolean = false): State[ChosenTranslation] = {

        def wrongQuestionsToRepeat = {
          val State(total, wrong) = state
          println(s"Has practicado $total palabras, ${wrong.size} incorrectas (${(total - wrong.size) * 100.0 / total}%)")
          val newQuestions = state.wrong.toArray
          shuffle(newQuestions)
          newQuestions.toList
        }

        questions match {
          case (word, ChosenTranslation(wd, qt)) :: rest =>
            if (!repeated) {
              println()
              qt.example.map(_.english).foreach(println)
              print(s"(${state.totalAsked + 1}/$totalCount) ${qt.speechPart}: (${qt.context.mkString(", ")}) ${qt.english.map(_.green).mkString(", ")} ")
            }
            StdIn.readLine() match {
              case "." => loop(round, wrongQuestionsToRepeat.size, State(), wrongQuestionsToRepeat)
              case `word` =>
                val exampleText = qt.example.map(_.spanish).map(": " + _).getOrElse("")
                println(s"Sí$exampleText")
                val now = new JDate
                val newBucket =
                  if (round == 0 && wd.bucket < 4) wd.bucket + bucketChange
                  else wd.bucket
                wordsColl.update(BSONDocument("_id" -> word), BSONDocument(
                  "$set" -> BSONDocument("lastCorrect" -> now, "bucket" -> newBucket),
                  "$inc" -> BSONDocument("correctCount" -> 1)
                ))
                loop(round, totalCount, state.right, rest)
              case answer if translationsByWord.getOrElse(answer, Nil)
                .exists(t => (t.english.toSet intersect qt.english.toSet).nonEmpty && t.baseSpeechPart == qt.baseSpeechPart) =>
                print(s"Sí, pero... ")
                loop(round, totalCount, state, questions, repeated = true)
              case answer =>
                val exampleText = qt.example.map(_.spanish).getOrElse("")
                println(s"${"NO".red}: $word\n$exampleText")
                val newBucket = 0 max (wd.bucket - bucketChange)
                wordsColl.update(BSONDocument("_id" -> word), BSONDocument(
                  "$set" -> BSONDocument("lastIncorrect" -> new JDate, "bucket" -> newBucket),
                  "$inc" -> BSONDocument("incorrectCount" -> 1)
                ))
                loop(round, totalCount, state.wrong(word, ChosenTranslation(wd.copy(bucket = newBucket), qt)), rest)
            }
          case Nil if state.wrong.nonEmpty =>
            loop(round + 1, wrongQuestionsToRepeat.size, State(), wrongQuestionsToRepeat)
          case _ =>
            state
        }
      }

      loop(0, questions.length, State(), questions.toList)
    }
  }
}

