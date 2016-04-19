import org.jsoup.Jsoup

/**
  * Created by ghik on 12.04.16.
  */
object Spanish {
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

  def main(args: Array[String]) {
    val irregularParticiples = Verbs.flatMap { verb =>
      Some(participle(verb)).filter(_ != regularParticiple(verb)).map((verb, _))
    }
    irregularParticiples.foreach(println)
  }

  val Verbs = List(
    "ser",
    "estar",
    "tener",
    "poder",
    "ver",
    "dormir",
    "hacer",
    "querer",
    "tocar",
    "nadar",
    "hablar",
    "saber",
    "llevar",
    "estudiar",
    "leer",
    "escribir",
    "jugar",
    "seguir",
    "encontrar",
    "mostrar",
    "probar",
    "mirar",
    "ayudar",
    "venir",
    "pesar",
    "bajar",
    "buscar",
    "cerrar",
    "recordar",
    "aceptar",
    "tomar",
    "visitar",
    "dar",
    "volver",
    "respetar",
    "pensar",
    "caber",
    "servir",
    "acabar",
    "comenzar",
    "empezar",
    "conocer",
    "parecer",
    "contar",
    "firmar",
    "culpar",
    "entregar",
    "importar",
    "incluir",
    "entrar",
    "llegar",
    "decir",
    "depender",
    "abrir",
    "requerir",
    "quedar",
    "soñar",
    "sentir",
    "llover",
    "dejar",
    "explicar",
    "pasar",
    "poner",
    "permanecer",
    "recibir",
    "salir",
    "decidir",
    "pedir",
    "ocurrir",
    "lograr",
    "responder",
    "perder",
    "terminar",
    "caer",
    "ganar",
    "iniciar",
    "obtener",
    "aparecer",
    "conseguir",
    "permitir",
    "contestar",
    "negar",
    "alcanzar",
    "insistir",
    "ofrecer",
    "considerar",
    "intentar",
    "sacar",
    "salvar",
    "comprar",
    "regresar",
    "llenar",
    "dudar",
    "parar",
    "poseer",
    "continuar",
    "mezclar",
    "utilizar",
    "añadir",
    "valer",
    "contener",
    "entender",
    "gastar",
    "derrotar",
    "reconocer",
    "cortar",
    "manejar",
    "rechazar",
    "costar",
    "consultar",
    "observar",
    "mejorar",
    "afectar",
    "reservar",
    "creer",
    "volar",
    "expresar",
    "crear",
    "andar",
    "usar",
    "terminar",
    "pertenecer",
    "tratar",
    "necesitar",
    "descansar",
    "esperar",
    "levantar",
    "tirar",
    "cantar",
    "curar",
    "odiar",
    "girar",
    "saltar",
    "ocupar",
    "partir",
    "alquilar",
    "evitar",
    "mantener",
    "cambiar",
    "establecer",
    "morir",
    "reducir",
    "olvidar",
    "abandonar",
    "aumentar",
    "superar",
    "controlar",
    "descubrir",
    "aplicar",
    "elegir",
    "demostrar",
    "recuperar",
    "vender",
    "defender",
    "regresar",
    "vencer",
    "resolver",
    "viajar",
    "explicar",
    "sumar",
    "escasear",
    "detener",
    "herir",
    "crecer"
  )
}
