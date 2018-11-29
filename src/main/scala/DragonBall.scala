// El fin de este objeto es agrupar las constantes y tipos
object DragonBall {

  /***************************** TIPOS *******************************/
  type ParGuerreros = (Guerrero, Guerrero)
  type PlanDeAtaque = List[Movimiento]
  /*************************** FIN TIPOS *****************************/

  /************************ RESULTADO PELEA **************************/
  sealed trait ResultadoPelea {
    def map(f: Movimiento): ResultadoPelea
    def flatMap(f: ParGuerreros => ResultadoPelea): ResultadoPelea
  }

  object ResultadoPelea {
    def apply(guerreros: (Guerrero, Guerrero)): ResultadoPelea = {
      guerreros match {
        case (GuerreroMuerto(_), GuerreroVivo(elOponente)) => Ganador(elOponente)
        case (GuerreroVivo(elGuerrero), GuerreroMuerto(_)) => Ganador(elGuerrero)
        case (GuerreroVivo(elGuerrero), GuerreroVivo(elOponente)) => Empate(elGuerrero, elOponente)
        case (GuerreroMuerto(elGuerrero), GuerreroMuerto(_)) => NoHayGanador
      }
    }
  }

  case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
    override def map(f: Movimiento): ResultadoPelea = this
    override def flatMap(f: ParGuerreros => ResultadoPelea): ResultadoPelea = this
  }

  case class Empate(guerrero: Guerrero, oponente: Guerrero) extends ResultadoPelea {
    override def map(f: Movimiento): ResultadoPelea = ResultadoPelea(f(guerrero, oponente))
    override def flatMap(f: ParGuerreros => ResultadoPelea): ResultadoPelea = {
      f((guerrero, oponente))
    }
  }

  case object NoHayGanador extends ResultadoPelea {
    override def map(f: Movimiento): ResultadoPelea = this
    override def flatMap(f: ParGuerreros => ResultadoPelea): ResultadoPelea = this
  }
  /********************** FIN RESULTADO PELEA *************************/

  /*************************** CONSTANTES *****************************/
  // Con la keyword val alcanza, se utiliza el final para dejar en claro que es constante
  // Se utiliza upper camel case por convención de Scala
  /*************************** Generales *****************************/
  final val EnergiaCritica = 1
  final val MultiplicadorEnergiaMono = 3
  final val MultiplicadorEnergiaNivelSSJ = 5
  /************************* Fin Generales ***************************/


  /*************************** Items *****************************/
  final val ProporcionalDanioArmaFilosa = 0.01
  final val DanioArmaFuegoNamekusein = 10
  final val DanioArmaFuegoHumano = 20
  final val MinimaEnergiaArmaRoma = 300
  /************************* Fin Items ***************************/


  /*************************** Movimientos *****************************/
  final val CargaKiHumano = 100
  final val CargaKiSaiyajin = 150
  /************************* Fin Movimientos ***************************/

  /***************************** Ataques ******************************/
  final val DanioGolpeNinja = 10
  final val DanioGolpeNinjaHumanoAndroide = 20
  final val MultiplicadorExplotarAndroide = 3
  final val MultiplicadorExplotarMonstruo = 2
  final val MultiplicadorDanioOndaEnergiaNoMonstruo = 2
  final val MultiplicadorDanioOndaEnergiaMonstruo = 0.5
  final val DanioGenkidama = 10
  /*************************** Fin Ataques ****************************/
  /************************* FIN CONSTANTES ***************************/

}

object CodeTest extends App{
  /*val goku = Guerrero(nombre = "Goku", inventario = Seq(),
    especie = Saiyajin(2000, 2000), habilidades = Seq(Movimientos.cargarKi, Movimientos.fusion,
      Movimientos.dejarseFajar, MuchosGolpesNinja.atacar, Genkidama.atacar, OndaEnergia(200).atacar))
  val cell = Guerrero(nombre = "Cell", inventario = Seq(Fuego, FotoLuna), especie = Monstruo(1500, 1500),
    habilidades = Seq(Movimientos.cargarKi, Explotar.atacar, OndaEnergia(100).atacar, MuchosGolpesNinja.atacar))
  val plan = goku.planDeAtaquePiolaContra(cell, 2)(CriterioMayorDanio)
  val resultado = goku.pelearContra(cell)(plan)*/
}
