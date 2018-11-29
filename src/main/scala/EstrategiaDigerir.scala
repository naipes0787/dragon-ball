import sun.reflect.generics.reflectiveObjects.NotImplementedException
import DragonBall.ParGuerreros

sealed trait EstrategiaDigerir {
  def digerir(atacante: Guerrero, atacado: Guerrero): ParGuerreros = {
    throw new NotImplementedException
  }
}

case object DigerirAndroide extends EstrategiaDigerir {

  override def digerir(guerrero: Guerrero, oponente: Guerrero): (Guerrero, Guerrero) = {
    val habilidadesNuevas = oponente.habilidades.diff(guerrero.habilidades)
    (guerrero.copy(habilidades = guerrero.habilidades ++ habilidadesNuevas), oponente.morir)
  }
}

case object DigerirUltimo extends EstrategiaDigerir {

  override def digerir(guerrero: Guerrero, oponente: Guerrero): (Guerrero, Guerrero) = (guerrero.copy(habilidades = oponente.habilidades), oponente.morir)

}