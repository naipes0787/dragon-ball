import sun.reflect.generics.reflectiveObjects.NotImplementedException
import DragonBall._

sealed class Criterio {
  def apply(guerreros: ParGuerreros, movimiento: Movimiento): Int = {
    throw new NotImplementedException
  }
}

case object CriterioMayorKiAtacante extends Criterio {
  // Devuelve la energía del atacante, luego en movimientoMasEfectivo hace un max sobre este valor,
  // quedandose con el movimiento que deja con mayor ki al atacante
  override def apply(guerreros: ParGuerreros, movimiento: Movimiento): Int = {
    val (atacanteAfectado, _) = movimiento(guerreros)
    atacanteAfectado.energia
  }
}

case object CriterioMayorDanio extends Criterio {
  // Devuelve el daño que se le ocasionó a atacado, luego en movimientoMasEfectivo hace un max sobre este valor,
  // quedandose con el movimiento que deja genera más daño
  override def apply(guerreros: ParGuerreros, movimiento: Movimiento): Int = {
    val (atacanteAfectado, atacadoAfectado) = movimiento(guerreros)
    guerreros._2.energia - atacadoAfectado.energia
  }
}

case object CriterioMayorVentajaDeKi extends Criterio {
  // Devuelve la diferencia de ki entre atacante y atacado, luego en movimientoMasEfectivo hace un max sobre
  // este valor, quedandose con el movimiento que deja con mayor ventaja (Mas diferencia de ki) al atacante
  override def apply(guerreros: ParGuerreros, movimiento: Movimiento): Int = {
    val (atacanteAfectado, oponenteAfectado) = movimiento(guerreros)
    atacanteAfectado.energia - oponenteAfectado.energia
  }
}


case object CriterioNegativo extends Criterio {
  override def apply(guerreros: (Guerrero, Guerrero), movimiento: Movimiento): Int = -1
}
