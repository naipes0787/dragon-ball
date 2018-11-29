import sun.reflect.generics.reflectiveObjects.NotImplementedException

sealed trait Item {
  //Metodo llamado cuando este item es usado por otro guerrero distinto al pasado por parametro
  //El atacante es necesario porque arma filosa pide realizar un calculo en base al ki del guerrero atacante
  def serUsadoEn(unGuerrero: Guerrero, atacante: Guerrero): Guerrero = {
    throw new NotImplementedException
  }

  def serUsadoPor(unGuerrero: Guerrero): Guerrero = {
    throw new NotImplementedException
  }
}

case object Fuego extends Item

case object FotoLuna extends Item

case object EsferaDragon extends Item

trait Arma extends Item

case object ArmaFilosa extends Arma {

  override def serUsadoEn(unGuerrero: Guerrero, atacante: Guerrero): Guerrero = unGuerrero.especie match {
    case Saiyajin(_,_,_,_,_) => unGuerrero.copy(especie = unGuerrero.perderCola.especie.setEnergia(
      nuevaEnergia = (unGuerrero.especie.energia - (
        (atacante.energia * DragonBall.ProporcionalDanioArmaFilosa).toInt
        )).max(0)))
    case Androide(_, _) => unGuerrero
    case _ => unGuerrero.copy(especie = unGuerrero.especie.setEnergia(nuevaEnergia = (unGuerrero.energia -
      (atacante.energia * DragonBall.ProporcionalDanioArmaFilosa).toInt).max(0)))
  }
}

case object ArmaFuego extends Arma {
  override def serUsadoEn(unGuerrero: Guerrero, atacante: Guerrero): Guerrero = unGuerrero.especie match {
    case Humano(_,_) => unGuerrero.copy(especie = unGuerrero.especie.setEnergia(
      unGuerrero.energia - DragonBall.DanioArmaFuegoHumano))
    case Namekusein(_,_) if unGuerrero.inconsciente => unGuerrero.copy(especie = unGuerrero.especie.setEnergia(
      unGuerrero.energia - DragonBall.DanioArmaFuegoNamekusein))
    case _ => unGuerrero
  }
}

case object ArmaRoma extends Arma {

  override def serUsadoEn(unGuerrero: Guerrero, atacante: Guerrero): Guerrero = unGuerrero.especie match {
    case Androide(_,_) => unGuerrero
    case _ => if (unGuerrero.energia < DragonBall.MinimaEnergiaArmaRoma) unGuerrero.dejarInconsciente
      else unGuerrero
  }
}

case object SemillaErmitanio extends Item {
  // Asumimos que la semilla del ermitaño quita el estado inconsciente
  override def serUsadoEn(unGuerrero: Guerrero, atacante: Guerrero): Guerrero =
    unGuerrero.sumarEnergia(unGuerrero.energiaMaxima).copy(inconsciente = false)

  override def serUsadoPor(unGuerrero: Guerrero): Guerrero = serUsadoEn(unGuerrero, unGuerrero)
}