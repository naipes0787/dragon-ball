class SinMovimientosException extends RuntimeException

import DragonBall._
import scala.util.Try

object GuerreroMuerto {
  def unapply(guerrero: Guerrero): Option[Guerrero] = {
    if (guerrero.estaMuerto) Some(guerrero) else None
  }
}

object GuerreroVivo {
  def unapply(guerrero: Guerrero): Option[Guerrero] = {
    if (guerrero.estaMuerto) None else Some(guerrero)
  }
}


object GuerreroMuertoOInconsciente {
  def unapply(guerrero: Guerrero): Option[Guerrero] = {
    if (guerrero.estaMuerto || guerrero.inconsciente) Some(guerrero) else None
  }
}

case class Guerrero(
                     nombre: String,
                     inventario: Seq[Item] = Seq(),
                     especie: Especie,
                     habilidades: Seq[Movimiento] = Seq(),
                     inconsciente: Boolean = false,
                     fajadasSeguidas: Int = 0
                   ) {


  def eliminarUnItem(itemAEliminar: Item) = {
    val index = inventario.indexOf(itemAEliminar)
    if (index < 0) inventario
    else if (index == 0) inventario.tail
    else {
      val (a, b) = inventario.splitAt(index)
      a ++ b.tail
    }
  }

  def energia: Int = especie.energia

  def energiaMaxima: Int = especie.energiaMaxima

  def estaMuerto: Boolean = energia == 0

  def estaMuertoOInconsciente: Boolean = estaMuerto || inconsciente

  def perderEnergia(cantidad: Int): Guerrero = {
    val nuevaEnergia = (energia - cantidad).max(0)
    if (nuevaEnergia == 0) morir
    else
      copy(especie = especie.setEnergia(nuevaEnergia))
  }


  def reiniciarFajadas: Guerrero = copy(fajadasSeguidas = 0)

  //Resta la cantidad de energia indicada, pero si es mayor a la energia actual, queda en 1
  def perderEnergiaSinMorir(cantidad: Int): Guerrero = copy(especie = especie.setEnergia((energia - cantidad).
    max(DragonBall.EnergiaCritica)))

  def perderCola: Guerrero = especie match {
    case Saiyajin(_, energiaMaxima, _, nivel, Some(MonoSaiyajin)) => copy(inconsciente = true, especie = Saiyajin(
      DragonBall.EnergiaCritica, energiaMaxima, tieneCola = false, nivel, None))
    case Saiyajin(_, energiaMaxima, _, nivel, transformacion) => copy(especie =
      Saiyajin(energia = DragonBall.EnergiaCritica, energiaMaxima = energiaMaxima, tieneCola = false, nivel = nivel,
        transformacion = transformacion))
    case _ => this
  }

  def eliminarNVecesItem(item: Item, veces: Int = 1): Guerrero = {
    var counter = 0
    val nuevoInventario = inventario.filter(i => {
      if (counter < veces && i == item) {
        counter += 1
        false
      } else true
    })
    copy(inventario = nuevoInventario)
  }

  //Este metodo es llamado para afectar al guerrero que uso el item
  def utilizarItem(item: Item): Guerrero = {
    item match {
      case ArmaFuego => copy(inventario = eliminarUnItem(Fuego))
      case SemillaErmitanio => copy(especie = especie.setEnergia(energiaMaxima))
      case EsferaDragon => eliminarNVecesItem(EsferaDragon, 7)
      case _ => this
    }
  }

  def dejarInconsciente: Guerrero = copy(inconsciente = true, fajadasSeguidas = 0)

  def salirDeInconsciencia: Guerrero = especie match {
    case Saiyajin(unaEnergia, unaEnergiaMaxima, _, _, Some(SuperSaiyajin)) => copy(inconsciente = false,
      especie = Saiyajin(unaEnergia, unaEnergiaMaxima, transformacion = None))
    case _ => copy(inconsciente = false)
  }

  def dejarseFajar: Guerrero = copy(fajadasSeguidas = fajadasSeguidas + 1)

  def sumarEnergia(aumentoKi: Int): Guerrero = copy(especie = especie.setEnergia(
    (energia + aumentoKi).min(energiaMaxima))
  )

  def morir: Guerrero = especie match {
    case Fusion(_, _, guerreroOriginal) => guerreroOriginal.copy(especie = guerreroOriginal.especie.setEnergia(0))
    case _ => copy(especie = especie.setEnergia(0))
  }

  def tieneItemCompleto(unItem: Item): Boolean = unItem match {
    case ArmaFuego => inventario.contains(unItem) && inventario.contains(Fuego)
    case _ => inventario.contains(unItem)
  }

  def tiene7Esferas: Boolean = {
    /* Podemos usar == para comparar ya que en Scala se utiliza para la igualdad de objetos,
    * y EsferaDragon es un case object */
    inventario.count(item => item == EsferaDragon) >= 7
  }

  def agregarHabilidad(movimiento: Movimiento): Guerrero = copy(habilidades = habilidades :+ movimiento)


  def kiPorLoMenosEnLaMitad: Boolean = energia >= (energia / 2)

  def tieneCola: Boolean = especie match {
    case Saiyajin(_, _, tieneCola, _, _) => tieneCola
    case _ => false
  }


  def subirNivel: Guerrero = especie match {
    case p: Saiyajin => copy(especie = p.copy(nivel = p.nivel + 1,
      energiaMaxima = DragonBall.MultiplicadorEnergiaNivelSSJ * energiaMaxima * (p.nivel + 1)))
    case _ => this
  }


  //Devuelve las habilidades cuyo criterio contra el oponente den un resultado no negativo
  private def getHabilidadesDeseables(oponente: Guerrero, criterio: Criterio) = habilidades.filter(m => {
    criterio match {
      case CriterioMayorVentajaDeKi => true // Este criterio acepta valores negativos
      case _ if !oponente.estaMuerto => criterio((this, oponente), m) > 0 // Si no esta muerto aplico el criterio
      case _ => true // Si esta muerto, ya le gane, todo sirve
    }
  })

  def movimientoMasEfectivo(oponente: Guerrero)(criterio: Criterio): Movimiento = {
    // Se toma siempre el que maximice el criterio, en caso de empate se tomará el último
    val deseables = getHabilidadesDeseables(oponente, criterio)
    if (deseables.nonEmpty) deseables.maxBy(habilidad => criterio((this, oponente), habilidad))
    else throw new SinMovimientosException
  }


  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Try[(Guerrero, Guerrero)] = Try {
    val (atacanteAfectado, oponenteAfectado) = movimiento(this, oponente)
    val contraataqueOponente = oponenteAfectado.movimientoMasEfectivo(atacanteAfectado)(CriterioMayorVentajaDeKi)
    val (defensor, atacante) = contraataqueOponente(oponenteAfectado, atacanteAfectado)
    (atacante, defensor)
  }

  // Primera implementación de Plan de Ataque
  /* def planDeAtaqueContra(unOponente: Guerrero, cantidadRounds: Int)(criterio: Criterio): PlanDeAtaque = {
     var guerrero = this
     var oponente = unOponente
     var plan = List[Movimiento]()
     for (_ <- 1 to cantidadRounds) {
       val movimiento = guerrero.movimientoMasEfectivo(oponente)(criterio)
       val (guerreroAfectado, oponenteAfectado) = guerrero.pelearUnRound(movimiento)(oponente)
       oponente = oponenteAfectado
       guerrero = guerreroAfectado
       plan ::= movimiento
     }
     plan
   }
 */
  // 3B -- Versión final de Plan de Ataque (Sin recursividad ni asignacion destructiva)
  def planDeAtaquePiolaContra(oponente: Guerrero, cantidadRounds: Int)(criterio: Criterio): Try[PlanDeAtaque] = Try {
    val rounds = List.range(0, cantidadRounds)
    val resultado = rounds.foldLeft((List[Movimiento](), this, oponente)) {
      case ((unPlan, unGuerrero, unOponente), _) =>
        val movimiento = unGuerrero.movimientoMasEfectivo(unOponente)(criterio)
        val resultadoRound = unGuerrero.pelearUnRound(movimiento)(unOponente)
        resultadoRound match {
          case scala.util.Success((guerreroAfectado, oponenteAfectado)) => (movimiento :: unPlan, guerreroAfectado, oponenteAfectado)
          case _ => throw new SinMovimientosException
        }
    }
    resultado._1
  }

  def pelearContra(unOponente: Guerrero)(planDeAtaque: Try[PlanDeAtaque]): ResultadoPelea = {
    planDeAtaque.map { plan =>
      plan.foldLeft(ResultadoPelea(this, unOponente)) { (res, mov) =>
        res match {
          case NoHayGanador => NoHayGanador
          case _ => res.flatMap(pelearConResultado(mov))
        }
      }
    }.getOrElse(NoHayGanador)
  }

  def pelearConResultado(movimiento: Movimiento)(guerreros: (Guerrero, Guerrero)): ResultadoPelea = {
    val (guerrero, oponente) = guerreros
    val resultadoRound = guerrero.pelearUnRound(movimiento)(oponente)
    resultadoRound.map(resultado => ResultadoPelea(resultado)).getOrElse(NoHayGanador)

    /* resultadoRound match {
       case scala.util.Success((guerreroAfectado, oponenteAfectado)) => ResultadoPelea(guerreroAfectado, oponenteAfectado)
     }*/
  }
}