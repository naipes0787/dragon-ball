import DragonBall.{ParGuerreros}


abstract class Movimiento {
  def ejecutarMovimiento(guerreros: ParGuerreros): ParGuerreros

  def chequearFajadas(guerreros: ParGuerreros): ParGuerreros = {
    val (g1, g2) = guerreros
    (g1.reiniciarFajadas, g2)
  }


  def apply(guerreros: ParGuerreros): ParGuerreros = ejecutarMovimiento(chequearFajadas(guerreros))

}


object DejarseFajar extends Movimiento {
  override def chequearFajadas(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = guerreros

  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = (guerreros._1.dejarseFajar, guerreros._2)

}

object CargarKi extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = guerreros._1.especie match {
    case Saiyajin(_, _, _, unNivel, Some(SuperSaiyajin)) => (guerreros._1.sumarEnergia(
      DragonBall.CargaKiSaiyajin * unNivel), guerreros._2)
    case Saiyajin(_, _, _, _, None) => (guerreros._1.sumarEnergia(DragonBall.CargaKiSaiyajin), guerreros._2)
    case Androide(_, _) => guerreros
    case _ => (guerreros._1.sumarEnergia(DragonBall.CargaKiHumano), guerreros._2)
  }
}


class UsarItemSobreOponente(item: Item) extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (atacante, atacado) = guerreros
    item match {
      case SemillaErmitanio if !atacante.estaMuerto && atacante.tieneItemCompleto(item) =>
        (atacante.utilizarItem(item), item.serUsadoEn(atacado, atacante))
      case _ if !atacante.estaMuertoOInconsciente && atacante.tieneItemCompleto(item) =>
        (atacante.utilizarItem(item), item.serUsadoEn(atacado, atacante))
      case _ => guerreros
    }
  }
}

class UsarItemSobreMi(item: Item) extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (atacante, atacado) = guerreros
    if (atacante.tieneItemCompleto(item)) (atacante.utilizarItem(item), atacado)
    else guerreros
  }
}

class ComerOponente(estrategiaDigerir: EstrategiaDigerir) extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (guerrero, oponente) = guerreros
    (guerrero.especie, oponente) match {
      case (Monstruo(_, _), _) if guerrero.energia > oponente.energia =>
        estrategiaDigerir.digerir(guerrero, oponente)
      case _ => (guerrero, oponente)
    }
  }
}


object ConvertirseEnMono extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (guerrero, oponente) = guerreros
    guerrero.especie match {
      case Saiyajin(_, _, true, _, None) if !guerrero.estaMuertoOInconsciente && guerrero.tieneItemCompleto(FotoLuna) =>
        (aplicarConversionMono(guerrero), oponente)
      case _ => guerreros
    }

  }
  private def aplicarConversionMono(guerrero: Guerrero): Guerrero = guerrero.copy(especie = Saiyajin(energiaMaxima = guerrero.energiaMaxima * DragonBall.MultiplicadorEnergiaMono, energia = guerrero.energiaMaxima *
    DragonBall.MultiplicadorEnergiaMono, transformacion = Some(MonoSaiyajin)))

}


object ConvertirseEnSSJ extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (guerrero, oponente) = guerreros
    guerrero.especie match {
      case Saiyajin(_, _, _, 1, _) if !guerrero.estaMuertoOInconsciente && guerrero.kiPorLoMenosEnLaMitad =>
        (aplicarConversionSsj(guerrero), oponente)
      case Saiyajin(_, _, _, _, Some(SuperSaiyajin)) if !guerrero.estaMuertoOInconsciente && guerrero.kiPorLoMenosEnLaMitad =>
        (guerrero.subirNivel, oponente)
      case _ => (guerrero, oponente)
    }
  }

  private def aplicarConversionSsj(guerrero: Guerrero): Guerrero = {
    guerrero.copy(especie = Saiyajin(energia = guerrero.especie.energia, energiaMaxima =
      guerrero.especie.energiaMaxima * DragonBall.MultiplicadorEnergiaNivelSSJ,
      tieneCola = guerrero.tieneCola, transformacion = Some(SuperSaiyajin)))
  }
}


object Fusionarse extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (guerrero, amigo) = guerreros
    (guerrero.especie, amigo.especie) match {
      case (Humano(_, _) | Namekusein(_, _) | Saiyajin(_, _, _, _, _),
      Humano(_, _) | Namekusein(_, _) | Saiyajin(_, _, _, _, _))
        if !guerrero.estaMuertoOInconsciente =>
        (aplicaFusion(guerrero, amigo), guerrero)
      case (_, _) => (guerrero, amigo)
    }
  }


  // Al nombre de la fusión le agregamos una concatenación de ambos nombres
  private def aplicaFusion(guerrero: Guerrero, amigo: Guerrero): Guerrero = {
    val habilidadesNuevas = amigo.habilidades.diff(guerrero.habilidades)
    guerrero.copy(nombre = guerrero.nombre + amigo.nombre.toLowerCase,
      habilidades = guerrero.habilidades ++ habilidadesNuevas,
      especie = Fusion(energia = guerrero.energia + amigo.energia, energiaMaxima =
        guerrero.energiaMaxima + amigo.energiaMaxima,
        guerreroOriginal = guerrero))
  }
}


class Magia(f: (Guerrero, Guerrero) => (Guerrero, Guerrero)) extends Movimiento {

  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (guerrero, oponente) = guerreros
    guerrero.especie match {
      case Namekusein(_, _) | Monstruo(_, _) if !guerrero.estaMuertoOInconsciente => f(guerrero, oponente)
      case _ if guerrero.tiene7Esferas && !guerrero.estaMuertoOInconsciente => f(guerrero.utilizarItem(
        EsferaDragon), oponente)
      case _ => (guerrero, oponente)
    }

  }
}

class Atacar(ataque: Ataque) extends Movimiento {
  override def ejecutarMovimiento(guerreros: (Guerrero, Guerrero)): (Guerrero, Guerrero) = {
    val (atacante, atacado) = guerreros
    atacante match {
      case GuerreroMuertoOInconsciente(a) => guerreros
      case _ => ataque.atacar(atacante, atacado)
    }
  }
}

