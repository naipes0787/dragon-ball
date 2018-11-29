import sun.reflect.generics.reflectiveObjects.NotImplementedException

abstract class Especie {


  def energia: Int

  def energiaMaxima: Int

  def setEnergia(nuevaEnergia: Int): Especie

  def unapply(guerrero: Guerrero): Option[Guerrero] =
    Some(guerrero).filter(p => p.especie == this)
}


case class Humano(energia: Int, energiaMaxima: Int) extends Especie {
  override def setEnergia(nuevaEnergia: Int): Humano = copy(energia = nuevaEnergia.max(0))

}

case class Saiyajin(energia: Int, energiaMaxima: Int,
                    tieneCola: Boolean = true,
                    nivel: Int = 1,
                    transformacion: Option[TransformacionSaiyajin] = None) extends Especie{
  override def setEnergia(nuevaEnergia: Int): Saiyajin = copy(energia = nuevaEnergia.max(0))
}

case class Androide(energia: Int, energiaMaxima: Int) extends Especie{
  override def setEnergia(nuevaEnergia: Int): Androide = copy(energia = nuevaEnergia.max(0))
}

case class Namekusein(energia: Int, energiaMaxima: Int) extends Especie{
  override def setEnergia(nuevaEnergia: Int): Namekusein = copy(energia = nuevaEnergia.max(0))
}

case class Monstruo(energia: Int, energiaMaxima: Int) extends Especie{
  override def setEnergia(nuevaEnergia: Int): Monstruo = copy(energia = nuevaEnergia.max(0))
}

case class Fusion(energia: Int, energiaMaxima: Int, guerreroOriginal: Guerrero) extends Especie{
  override def setEnergia(nuevaEnergia: Int): Fusion = copy(energia = nuevaEnergia.max(0))
}



