
import DragonBall.ParGuerreros
import sun.reflect.generics.reflectiveObjects.NotImplementedException

trait Ataque {
  def atacar(atacante: Guerrero, atacado: Guerrero): ParGuerreros = {
    throw new NotImplementedException
  }
}


trait AtaqueEnergia extends Ataque {

  def atacarConEnergiaA(guerrero: Guerrero, poderEnergico: Int): Guerrero = guerrero.especie match {
    case Androide(_, _) => guerrero.sumarEnergia(poderEnergico)
    case _ => guerrero.perderEnergia(poderEnergico)
  }
}

trait AtaqueFisico extends Ataque


case object MuchosGolpesNinja extends AtaqueFisico {
  override def atacar(atacante: Guerrero, atacado: Guerrero): (Guerrero, Guerrero) = {
    (atacante.especie, atacado.especie) match {
      case (Humano(_, _), Androide(_, _)) => (atacante.copy(especie = atacante.especie.
        setEnergia(atacante.energia - DragonBall.DanioGolpeNinja)), atacado)
      case _ => if (atacante.energia > atacado.energia)
        (atacante, atacado.perderEnergia(DragonBall.DanioGolpeNinjaHumanoAndroide))
      else (atacante.perderEnergia(DragonBall.DanioGolpeNinjaHumanoAndroide), atacado)
    }
  }
}


case object Explotar extends AtaqueFisico {

  override def atacar(atacante: Guerrero, atacado: Guerrero): (Guerrero, Guerrero) = (atacante.especie, atacado.especie) match {
    case (Androide(_, _), Namekusein(_, _)) => (atacante.morir, atacado.
      perderEnergiaSinMorir(atacante.energia * DragonBall.MultiplicadorExplotarAndroide))
    case (Monstruo(_, _), Namekusein(_, _)) => (atacante.morir, atacado.
      perderEnergiaSinMorir(atacante.energia * DragonBall.MultiplicadorExplotarMonstruo))
    case (Androide(_, _), _) => (atacante.morir, atacado.
      perderEnergia(atacante.energia * DragonBall.MultiplicadorExplotarAndroide))
    case (Monstruo(_, _), _) => (atacante.morir, atacado.
      perderEnergia(atacante.energia * DragonBall.MultiplicadorExplotarMonstruo))
    case _ => (atacante, atacado)
  }
}


case class OndaEnergia(energiaRequerida: Int) extends AtaqueEnergia {

  override def atacar(atacante: Guerrero, atacado: Guerrero): (Guerrero, Guerrero) = (atacante.especie, atacado.especie) match {
    case (_, Monstruo(_, _)) if tieneSuficienteEnergia(atacante) =>
      (atacante.perderEnergia(energiaRequerida), atacarConEnergiaA(atacado, (energiaRequerida *
        DragonBall.MultiplicadorDanioOndaEnergiaMonstruo).toInt))
    case _ if tieneSuficienteEnergia(atacante) =>
      (atacante.perderEnergia(energiaRequerida), atacarConEnergiaA(atacado, energiaRequerida *
        DragonBall.MultiplicadorDanioOndaEnergiaNoMonstruo))
    case _ => (atacante, atacado)
  }

  private def tieneSuficienteEnergia(guerrero: Guerrero): Boolean = guerrero.energia >= energiaRequerida
}

case object Genkidama extends AtaqueEnergia {

  override def atacar(atacante: Guerrero, atacado: Guerrero): (Guerrero, Guerrero) = (atacante, atacarConEnergiaA(atacado, Math.pow(DragonBall.DanioGenkidama, atacante.fajadasSeguidas).toInt))

}
