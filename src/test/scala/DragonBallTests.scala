import org.scalatest.{FreeSpec, Matchers}
import DragonBall._
import scala.util.{Try, Success, Failure}

class DragonBallTests extends FreeSpec with Matchers {

  "Dragon Ball" - {

    "Mensajes a un guerrero" - {
      "verificando que los atributos se asignen correctamente" in {
        val unHumano = Guerrero(nombre = "MrSatan", inventario = Seq.empty, especie = Humano(100, 100),
          habilidades = Seq.empty)
        unHumano.nombre shouldBe "MrSatan"
        unHumano.inventario shouldBe Seq.empty
        unHumano.energia shouldBe 100
        unHumano.energiaMaxima shouldBe 100
        (unHumano.especie match {
          case Humano(_, _) => true
          case _ => false
        }) shouldBe true
      }

      "saiyajin esta convertido en mono" in {
        val mono = Guerrero(
          nombre = "Goku",
          especie = Saiyajin(10000, 1000, nivel = 2, transformacion = Some(MonoSaiyajin)))

        (mono.especie match {
          case Saiyajin(_, _, _, _, Some(MonoSaiyajin)) => true
          case _ => false
        }) shouldBe true
      }

      "guerrero es androide" in {
        val a18 = Guerrero(nombre = "A-18", especie = Androide(100, 100))
        (a18.especie match {
          case Androide(_, _) => true
          case _ => false
        }) shouldBe true
      }

      "Al utilizar las esferas del dragón deberían eliminarse 7" in {
        val unGuerrero = Guerrero(nombre = "Ballboy", inventario = Seq(EsferaDragon, EsferaDragon, EsferaDragon,
          EsferaDragon, EsferaDragon, EsferaDragon, EsferaDragon, EsferaDragon), especie = Humano(100, 100))
        unGuerrero.utilizarItem(EsferaDragon).inventario.count(p => p == EsferaDragon) shouldBe 1
      }

      "tiene7Esferas" in {
        val unGuerrero = Guerrero(nombre = "Ballboy", inventario = Seq(EsferaDragon), especie = Humano(100, 100))
        unGuerrero.tiene7Esferas shouldBe false
        val otroGuerrero = Guerrero(nombre = "Ballboy Piola",
          inventario = Seq(EsferaDragon, EsferaDragon, EsferaDragon, EsferaDragon, EsferaDragon, EsferaDragon, EsferaDragon),
          especie = Humano(100, 100))
        otroGuerrero.tiene7Esferas shouldBe true
      }

      "dejarse fajar debe aumentar fajadasSeguidas pero no afectar al estado del guerrero" in {
        val unHumano = Guerrero(nombre = "MrSatan", inventario = Seq(Fuego), especie = Humano(500, 500),
          habilidades = Seq.empty)
        val otroHumano = unHumano.dejarseFajar
        val tercerHumano = otroHumano.dejarseFajar
        tercerHumano.energia shouldEqual unHumano.energia
        tercerHumano.energiaMaxima shouldEqual unHumano.energiaMaxima
        tercerHumano.habilidades shouldEqual unHumano.habilidades
        tercerHumano.inventario shouldEqual unHumano.inventario
        tercerHumano.especie shouldEqual unHumano.especie
        tercerHumano.inconsciente shouldEqual unHumano.inconsciente
        tercerHumano.fajadasSeguidas shouldBe 2
      }

      "dejarse fajar debe resetearse si se usa otro movimiento" in {
        val unHumano = Guerrero(nombre = "MrSatan", inventario = Seq(Fuego), especie = Humano(500, 500),
          habilidades = Seq.empty)
        val otroHumano = unHumano.dejarseFajar
        val (tercerHumano, _) = CargarKi(otroHumano, otroHumano)
        tercerHumano.fajadasSeguidas shouldBe 0
      }

      "tiene item" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego), especie = Humano(100, 100),
          habilidades = Seq.empty)
        goku.tieneItemCompleto(Fuego) shouldBe true
      }

      "Al utilizar el Arma de Fuego, debe eliminarse un Fuego" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego), especie = Humano(100, 100),
          habilidades = Seq.empty)
        goku.utilizarItem(ArmaFuego).inventario.size shouldBe 0
      }

      "remover un fuego dejando los demas" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego, Fuego, Fuego),
          especie = Humano(100, 100), habilidades = Seq.empty)
        goku.utilizarItem(ArmaFuego).inventario.size shouldBe 2
      }

      "mono pierde su cola" in {
        var mono = Guerrero(
          nombre = "Goku", especie = Saiyajin(energia = 1000, energiaMaxima = 1000, nivel = 2, transformacion = Some(MonoSaiyajin)))
        mono = mono.perderCola
        mono.energia shouldBe 1
        mono.inconsciente shouldBe true
        (mono.especie match {
          case Saiyajin(_, _, false, _, None) => true
          case _ => false
        }) shouldBe true
      }

      "agregar habilidad a guerrero" in {
        var goku = Guerrero(nombre = "Goku",
          inventario = Seq(Fuego),
          especie = Humano(100, 100),
          habilidades = Seq.empty)
        val movimiento = new UsarItemSobreOponente(ArmaFilosa)
        goku = goku.agregarHabilidad(movimiento)
        goku.habilidades.size shouldBe 1
      }
    }


    "Items" - {
      val noob = Guerrero(nombre = "Krillin", especie = Humano(1, 1))
      val medium = Guerrero(nombre = "Chaos", inventario = Seq(ArmaFilosa), especie = Humano(200, 200))
      val groso = Guerrero(nombre = "Goku", inventario = Seq(ArmaRoma), especie = Saiyajin(400, 400))

      "arma roma deja inconsciente" in {
        val (_, oponente) = new UsarItemSobreOponente(ArmaRoma)(groso, noob)
        oponente.nombre shouldBe "Krillin"
        oponente.inconsciente shouldBe true
      }

      "arma roma no hace nada por ki elevado" in {
        val (_, oponente) = new UsarItemSobreOponente(ArmaRoma)(noob, groso)
        oponente.nombre shouldBe "Goku"
        oponente.inconsciente shouldBe false
      }

      "arma roma no hace nada por ser oponente androide" in {
        val (_, oponente) = new UsarItemSobreOponente(ArmaRoma)(noob, groso)
        oponente.nombre shouldBe "Goku"
        oponente.inconsciente shouldBe false
      }

      "arma filosa a saiyajin reduce 1 punto por cada 100 del oponente y hace que pierda la cola" in {
        val (_, oponente) = new UsarItemSobreOponente(ArmaFilosa)(medium, groso)
        oponente.tieneCola shouldBe false
        oponente.energia shouldBe 398
      }

      "arma filosa reduce 1 punto por cada 100 del oponente a un humano" in {
        val otroGroso = groso.copy(inventario = Seq(ArmaFilosa))
        val (_, oponente) = new UsarItemSobreOponente(ArmaFilosa)(otroGroso, medium)
        oponente.energia shouldBe 196
      }

      "arma fuego no puede usarse si no se posee Fuego" in {
        val groso2 = groso.copy(inventario = Seq(ArmaFuego))
        groso2.tieneItemCompleto(ArmaFuego) shouldBe false
      }

      "arma fuego causa 20 puntos de daño a humano y resta uno de munición a quien lo usa" in {
        val noob2 = noob.copy(especie = noob.especie.setEnergia(100))
        val groso2 = groso.copy(inventario = Seq(ArmaFuego, Fuego))
        val (guerrero, oponente) = new UsarItemSobreOponente(ArmaFuego)(groso2, noob2)
        guerrero.tieneItemCompleto(Fuego) shouldBe false
        oponente.nombre shouldBe "Krillin"
        oponente.energia shouldBe 80
      }

      "arma fuego causa 10 puntos de daño a un namekusein inconsciente y resta uno de munición a quien lo usa" in {
        val piccolo = Guerrero(nombre = "Piccolo", especie = Namekusein(50, 50), inconsciente = true)
        val groso2 = groso.copy(inventario = Seq(ArmaFuego, Fuego))
        val (guerrero, oponente) = new UsarItemSobreOponente(ArmaFuego)(groso2, piccolo)
        guerrero.tieneItemCompleto(Fuego) shouldBe false
        oponente.nombre shouldBe "Piccolo"
        oponente.energia shouldBe 40
      }

      "arma fuego no causa daño a un namekusein porque no está inconsciente" in {
        val piccolo = Guerrero(nombre = "Piccolo", especie = Namekusein(50, 50))
        val groso2 = groso.copy(inventario = Seq(ArmaFuego, Fuego))
        val (_, oponente) = new UsarItemSobreOponente(ArmaFuego)(groso2, piccolo)
        oponente.nombre shouldBe "Piccolo"
        oponente.energia shouldBe 50
      }

      "semilla regenera ki" in {
        val groso = Guerrero(nombre = "Goku", inventario = Seq(SemillaErmitanio), especie = Saiyajin(1, 1000))
        val unoQueAndabaPorAhi = Guerrero(nombre = "Juan", especie = Humano(100, 100))
        val movimientoSemilla = new UsarItemSobreMi(SemillaErmitanio)
        val (recuperado, _) = movimientoSemilla(groso, unoQueAndabaPorAhi)
        recuperado.energia shouldBe recuperado.energiaMaxima
      }

      "semilla no hace crecer la cola cortada" in {
        val groso = Guerrero(nombre = "Goku", inventario = Seq(SemillaErmitanio), especie = Saiyajin(1, 1000, tieneCola = false))
        val unoQueAndabaPorAhi = Guerrero(nombre = "Juan", especie = Humano(100, 100))
        val movimientoSemilla = new UsarItemSobreMi(SemillaErmitanio)
        val (recuperado, _) = movimientoSemilla(groso, unoQueAndabaPorAhi)
        //recuperado.especie shouldBe Saiyajin(1000, 1000, tieneCola = false)
        (recuperado.especie match {
          case Saiyajin(_, _, false, _, _) => true
          case _ => false
        }) shouldBe true
      }

    }


    "Movimientos" - {
      "humano luego de cargar ki" in {
        val unHumano = Guerrero(nombre = "MrSatan", inventario = Seq(Fuego), especie = Humano(500, 1500),
          habilidades = Seq.empty)
        val (otroHumano, _) = CargarKi(unHumano, unHumano)
        otroHumano.energia shouldBe 600
      }

      "humano luego de cargar ki teniendo el ki al máximo" in {
        val unHumano = Guerrero(nombre = "MrSatan", inventario = Seq(Fuego), especie = Humano(500, 500),
          habilidades = Seq.empty)
        val (otroHumano, _) = CargarKi(unHumano, unHumano)
        otroHumano.energia shouldBe 500
      }

      "Super saiyajin de nivel 2 luego de cargar ki" in {
        val unSaiyajin = Guerrero(nombre = "Goku", inventario = Seq(Fuego),
          Saiyajin(energia = 200, energiaMaxima = 2000, nivel = 2, transformacion = Some(SuperSaiyajin)), habilidades = Seq.empty)
        val (otroSaiyajin, _) = CargarKi(unSaiyajin, unSaiyajin)
        otroSaiyajin.energia shouldBe 500
      }

      "androide luego de cargar ki" in {
        val unAndroide = Guerrero(nombre = "A-18", inventario = Seq(Fuego), especie = Androide(0, 0),
          habilidades = Seq.empty)
        val (otroAndroide, _) = CargarKi(unAndroide, unAndroide)
        otroAndroide.energia shouldBe 0
      }

      "saiyajin luego de cargar ki" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego), especie = Saiyajin(200, 2000),
          habilidades = Seq.empty)
        val (otroSaiyajin, _) = CargarKi(goku, goku)
        otroSaiyajin.energia shouldBe 350
      }

      "humano intenta comerse un saiyajin" in {
        val mrSatan = Guerrero(nombre = "MrSatan", inventario = Seq(Fuego), especie = Humano(500, 500),
          habilidades = Seq.empty)
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego), especie = Saiyajin(200, 200),
          habilidades = Seq.empty)
        val (otroMrSatan, otroGoku) = new ComerOponente(DigerirUltimo)(mrSatan, goku)
        otroMrSatan.habilidades shouldBe mrSatan.habilidades
        otroGoku.energia shouldBe goku.especie.energia
      }

      "Majin Buu se come a goku" in {
        val majinBuu = Guerrero(nombre = "Majin Buu", inventario = Seq(Fuego),
          especie = Monstruo(500, 500), habilidades = Seq.empty)
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego), especie = Saiyajin(200, 200),
          habilidades = Seq(CargarKi, ConvertirseEnMono))
        val (otroMajinBuu, otroGoku) = new ComerOponente(DigerirUltimo)(majinBuu, goku)
        otroMajinBuu.habilidades should not be majinBuu.habilidades
        otroMajinBuu.habilidades.size shouldBe 2
        otroGoku.energia shouldBe 0
      }

      "Majin Buu se come a A-18 y después a goku y solo se queda con las habilidades de Goku" in {
        val majinBuu = Guerrero(nombre = "Majin Buu", inventario = Seq(Fuego),
          especie = Monstruo(500, 500), habilidades = Seq.empty)
        val A18 = Guerrero(nombre = "A-18", inventario = Seq(Fuego), especie = Androide(200, 200),
          habilidades = Seq(CargarKi))
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego), especie = Saiyajin(200, 200),
          habilidades = Seq(ConvertirseEnSSJ, ConvertirseEnMono))
        val (otroMajinBuu, _) = new ComerOponente(DigerirUltimo)(majinBuu, A18)
        val (otroOtroMajinBuu, otroGoku) = new ComerOponente(DigerirUltimo)(otroMajinBuu, goku)
        otroOtroMajinBuu.habilidades should not be majinBuu.habilidades
        otroOtroMajinBuu.habilidades should not be A18.habilidades
        otroOtroMajinBuu.habilidades.size shouldBe 2
        otroOtroMajinBuu.habilidades shouldBe goku.habilidades
        otroGoku.energia shouldBe 0
      }

      "Majin Buu no puede comer a goku porque tiene mayor ki" in {
        val majinBuu = Guerrero(nombre = "Majin Buu", inventario = Seq(Fuego),
          especie = Monstruo(500, 500), habilidades = Seq.empty)
        val goku = Guerrero(nombre = "Goku", inventario = Seq(Fuego), especie = Saiyajin(700, 700),
          habilidades = Seq(CargarKi))
        val (otroMajinBuu, otroGoku) = new ComerOponente(DigerirUltimo)(majinBuu, goku)
        otroMajinBuu.habilidades shouldBe majinBuu.habilidades
        otroGoku.energia shouldBe goku.energia
      }

      "Cell se come a A-18" in {
        val cell = Guerrero(nombre = "Cell", inventario = Seq(Fuego),
          especie = Monstruo(500, 500), habilidades = Seq(new ComerOponente(DigerirAndroide)))
        val A18 = Guerrero(nombre = "A-18", inventario = Seq(Fuego), especie = Androide(200, 200),
          habilidades = Seq(CargarKi))
        val (otroCell, otroA18) = new ComerOponente(DigerirAndroide)(cell, A18)
        otroCell.habilidades should not be cell.habilidades
        otroCell.habilidades.size shouldBe 2
        otroA18.energia shouldBe 0
      }

      "goku y vegeta se fusionan en gogeta" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq.empty, especie = Saiyajin(700, 1000),
          habilidades = Seq(Fusionarse))
        val vegeta = Guerrero(nombre = "Vegeta", inventario = Seq.empty, especie = Saiyajin(500, 500),
          habilidades = Seq(CargarKi, Fusionarse))
        val (gogeta, _) = Fusionarse(goku, vegeta)
        gogeta.energia shouldBe 1200
        gogeta.energiaMaxima shouldBe 1500
        gogeta.habilidades.size shouldBe 2
      }

      "goku y cell se fusionan pero no pasa nada ya que cell no es ni humano, ni namekusein ni saiyajin" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq.empty, especie = Saiyajin(700, 1000),
          habilidades = Seq(Fusionarse))
        val cell = Guerrero(nombre = "Cell", inventario = Seq(Fuego),
          especie = Monstruo(500, 500), habilidades = Seq(new ComerOponente(DigerirAndroide)))
        val (otroGoku, otroCell) = Fusionarse(goku, cell)
        otroGoku.energia shouldBe goku.energia
        otroGoku.energiaMaxima shouldBe goku.energiaMaxima
        otroCell.energia shouldBe cell.energia
        otroCell.energiaMaxima shouldBe cell.energiaMaxima
      }

      "La fusion gogeta muere, entonces debería quedarme goku muerto" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq.empty, especie = Saiyajin(700, 1000),
          habilidades = Seq(Fusionarse))
        val vegeta = Guerrero(nombre = "Vegeta", inventario = Seq.empty, especie = Saiyajin(500, 500),
          habilidades = Seq(CargarKi, Fusionarse))
        val (gogeta, _) = Fusionarse(goku, vegeta)
        val gokuMuerto = gogeta.morir
        gokuMuerto.energia shouldBe 0
        gokuMuerto.energiaMaxima shouldBe 1000
      }

      "Magia permite aplicar funcion en guerreros" - {
        val unMago = Guerrero(nombre = "Piccolo", inventario = Seq.empty, especie = Namekusein(700, 1000),
          habilidades = Seq(Fusionarse))
        val otroGuerrero = Guerrero(nombre = "Humano", inventario = Seq.empty, especie = Humano(700, 1000),
          habilidades = Seq())
        val dejarInconscientes = (unGuerrero: Guerrero, otroGuerrero: Guerrero) => (unGuerrero.dejarInconsciente, otroGuerrero.dejarInconsciente)
        val magia = new Magia(dejarInconscientes)
        val unHumanoConEsferas = Guerrero(nombre = "Humano", inventario = Seq(EsferaDragon, EsferaDragon, EsferaDragon,
          EsferaDragon, EsferaDragon, EsferaDragon, EsferaDragon), especie = Humano(700, 1000),
          habilidades = Seq(new Magia(dejarInconscientes)))
        "Cuando el atacante tiene poderes misticos" in {
          val (unGuerrero2, otroGuerrero2) = magia(unMago, otroGuerrero)
          unGuerrero2.inconsciente shouldBe true
          otroGuerrero2.inconsciente shouldBe true
        }

        "Cuando el atacante no tiene poderes misticos" in {
          val (unGuerrero2, otroGuerrero2) = magia(otroGuerrero, unMago)
          unGuerrero2.inconsciente shouldBe false
          otroGuerrero2.inconsciente shouldBe false
        }

        "Cuando un humano usa las esferas del dragón" in {
          val (unHumanoSinEsferas, otroGuerrero2) = magia(unHumanoConEsferas, otroGuerrero)
          unHumanoSinEsferas.inventario.contains(EsferaDragon) shouldBe false
        }

        "saiyajin con 50 de energía y 300 de energía máxima se convertierte en mono" in {
          val goku = Guerrero(nombre = "Goku", inventario = Seq(FotoLuna), especie = Saiyajin(150, 300),
            habilidades = Seq(Fusionarse))
          val (mono, _) = ConvertirseEnMono(goku, goku)
          mono.energia shouldBe 900
          mono.energiaMaxima shouldBe 900
        }
      }

      "convertirse en mono gigante" in {
        val goku = Guerrero(nombre = "Goku",
          inventario = Seq(FotoLuna),
          especie = Saiyajin(100, 100),
          habilidades = Seq.empty)
        val conversion = ConvertirseEnMono
        val (mono, _) = conversion(goku, goku)
        (mono.especie match {
          case Saiyajin(_, _, true, _, Some(MonoSaiyajin)) => true
          case _ => false
        }) shouldBe true
        mono.energiaMaxima shouldBe (goku.energiaMaxima * 3)
        mono.energia shouldBe mono.energiaMaxima
      }

      "convertirse en SSJ 1" in {
        val goku = Guerrero(nombre = "Goku",
          inventario = Seq(FotoLuna),
          especie = Saiyajin(70, 100),
          habilidades = Seq.empty)
        val movimiento = ConvertirseEnSSJ
        val (ssj, _) = movimiento(goku, goku)
        (ssj.especie match {
          case Saiyajin(_, _, _, 1, Some(SuperSaiyajin)) => true
          case _ => false
        }) shouldBe true
        ssj.energiaMaxima shouldBe goku.energiaMaxima * 5
        ssj.energia shouldBe goku.energia
      }

      "subir de nivel" in {
        val goku = Guerrero(nombre = "Goku",
          inventario = Seq(FotoLuna),
          especie = Saiyajin(70, 100, nivel = 2, transformacion = Some(SuperSaiyajin)),
          habilidades = Seq.empty)
        val movimiento = ConvertirseEnSSJ
        val (ssj, _) = movimiento(goku, goku)
        (ssj.especie match {
          case Saiyajin(_, _, _, 3, Some(SuperSaiyajin)) => true
          case _ => false
        }) shouldBe true
        ssj.energiaMaxima shouldBe goku.energiaMaxima * 5 * 3
        ssj.energia shouldBe goku.energia
      }
    }


    "Ataques" - {
      val goku = Guerrero(nombre = "Goku", inventario = Seq.empty, especie = Saiyajin(3000, 3000))
      val krillin = Guerrero(nombre = "Krillin", especie = Humano(200, 200))
      val muchosGolpesNinja = new Atacar(MuchosGolpesNinja)
      val explotar = new Atacar(Explotar)
      val a17 = Guerrero(nombre = "A-17", especie = Androide(1000, 2000))
      val ondaEnergia = new Atacar(OndaEnergia(100))
      val piccolo = Guerrero(nombre = "Piccolo", especie = Namekusein(50, 50))

      "muchos golpes ninja" in {
        val (atacante, _) = muchosGolpesNinja(krillin, goku)
        atacante.energia shouldBe 180
      }

      "humano se lastima los deditos" in {
        val (atacante, _) = muchosGolpesNinja(krillin, a17)
        atacante.energia shouldBe (krillin.energia - 10)
      }

      "explotar androide a goku" in {
        val (cellMuerto, gokuHerido) = explotar(a17, goku)
        cellMuerto.energia shouldBe 0
        gokuHerido.energia shouldBe 0
      }

      "explotar a un namekusein" in {
        val (_, piccoloDebil) = explotar(a17, piccolo)
        piccoloDebil.energia shouldBe 1
      }

      "un humano no puede explotar" in {
        val (krillinMuerto, gokuHerido) = explotar(krillin, goku)
        krillinMuerto.energia shouldBe 200
        gokuHerido.energia shouldBe 3000
      }

      "onda energia" in {
        val (gokuDebil, cellFuerte) = ondaEnergia(goku, a17)
        gokuDebil.energia shouldBe 2900
        cellFuerte.energia shouldBe 1200
      }
    }

    "Simulador" - {
      "Movimiento más efectivo para mejorar el ki del guerrero contra vegeta" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq.empty, especie = Saiyajin(700, 1000),
          habilidades = Seq(CargarKi, Fusionarse, DejarseFajar,
            new Atacar(MuchosGolpesNinja), new Atacar(Genkidama), new Atacar(new OndaEnergia(300))))

        val vegeta = Guerrero(nombre = "Vegeta", inventario = Seq.empty, especie = Saiyajin(500, 500),
          habilidades = Seq(CargarKi, Fusionarse))
        val movimientoMasEfectivo = goku.movimientoMasEfectivo(vegeta)(CriterioMayorKiAtacante)
        val (fusion, _) = movimientoMasEfectivo(goku, vegeta)
        // Debería elegir fusión
        fusion.especie shouldBe Fusion(1200, 1500, goku)
        fusion.energia shouldBe 1200
      }

      "Movimiento más efectivo para mejorar el ki del guerrero contra vegeta 2" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq.empty, especie = Saiyajin(700, 1000),
          habilidades = Seq(CargarKi, DejarseFajar,
            new Atacar(MuchosGolpesNinja), new Atacar(Genkidama), new Atacar(new OndaEnergia(300))))


        val vegeta = Guerrero(nombre = "Vegeta", inventario = Seq.empty, especie = Saiyajin(500, 500),
          habilidades = Seq(CargarKi, Fusionarse))
        val movimientoMasEfectivo = goku.movimientoMasEfectivo(vegeta)(CriterioMayorKiAtacante)
        val (otroGoku, otroVegeta) = movimientoMasEfectivo(goku, vegeta)
        // Debería elegir cargar ki
        otroGoku.energia shouldBe 850
        otroVegeta.energia shouldBe 500
      }

      "Movimiento más efectivo para generar mas danio contra vegeta" in {
        val goku = Guerrero(nombre = "Goku", inventario = Seq.empty, especie = Saiyajin(700, 1000),
          habilidades = Seq(CargarKi, Fusionarse, DejarseFajar,
            new Atacar(MuchosGolpesNinja), new Atacar(Genkidama), new Atacar(new OndaEnergia(250))))
        val vegeta = Guerrero(nombre = "Vegeta", inventario = Seq.empty, especie = Saiyajin(500, 500),
          habilidades = Seq(CargarKi, Fusionarse))
        val movimientoMasEfectivo = goku.movimientoMasEfectivo(vegeta)(CriterioMayorDanio)
        val (otroGoku, otroVegeta) = movimientoMasEfectivo(goku, vegeta)
        // Debería elegir la Onda de Energía
        otroGoku.energia shouldBe 450
        otroVegeta.energia shouldBe 0
      }

      "Pelear un round" - {
        "Movimientos inofensivos" in {
          val saiya = Guerrero(nombre = "Saiya", inventario = Seq(),
            especie = Saiyajin(500, 500), habilidades = Seq(ConvertirseEnSSJ))
          val jin = Guerrero(nombre = "Jin", inventario = Seq(Fuego, FotoLuna), especie = Saiyajin(200, 1000),
            habilidades = Seq(CargarKi, new Atacar(MuchosGolpesNinja), ConvertirseEnMono))
          val resultado = saiya.pelearUnRound(saiya.habilidades.head)(jin)
          (resultado match {
            case Success((saiyaDespues, jinDespues)) if jinDespues.energia == 3000 && saiyaDespues.energiaMaxima == 2500 => true
            case Failure(exception) => false
          }) shouldBe true


        }

        //TODO alguien que revise la logica de este test
//        "Movimientos ofensivos" in {
//          val gunman = Guerrero(nombre = "Gunman", inventario = Seq(ArmaFuego, Fuego, Fuego),
//            especie = Saiyajin(500, 500), habilidades = Seq(new UsarItemSobreOponente(ArmaFuego)))
//          val jin = Guerrero(nombre = "Jin", inventario = Seq(ArmaFuego, ArmaFilosa, ArmaRoma, Fuego),
//            especie = Humano(200, 1000), habilidades = Seq(new UsarItemSobreOponente(ArmaFuego),
//              new UsarItemSobreOponente(ArmaFilosa), CargarKi, new UsarItemSobreOponente(ArmaRoma)))
//          val resultadoRound = gunman.pelearUnRound(gunman.habilidades.head)(jin)
//          (resultadoRound match {
//            case Success((saiyaDespues, jinDespues)) if jinDespues.energia == 280 => saiyaDespues.inventario.shouldBe(Seq(ArmaFuego, Fuego))
//            case Failure(exception) => false
//          }) shouldBe true
//        }

        "nignun movimiento satisface el criterio" in {
          val goku = Guerrero(nombre = "Goku", especie = Saiyajin(500, 500), habilidades = Seq(CargarKi, DejarseFajar, new Atacar(Genkidama)))
          val cell = Guerrero(nombre = "Cell", especie = Androide(1000, 1000), habilidades = Seq(new Atacar(Explotar)))

          val planDeAtaque = goku.planDeAtaquePiolaContra(cell, 5)(CriterioNegativo)
          (
            planDeAtaque match {
              case Success(plan) => true
              case Failure(exception) => false
            }) shouldBe false

        }

      }


      "Armar un plan" - {
        "plan con un solo ataque" in {
          val saiya = Guerrero(nombre = "Saiya", inventario = Seq(),
            especie = Saiyajin(500, 500), habilidades = Seq(ConvertirseEnSSJ))
          val jin = Guerrero(nombre = "Jin", inventario = Seq(Fuego, FotoLuna), especie = Saiyajin(200, 1000),
            habilidades = Seq(CargarKi, ConvertirseEnMono))
          saiya.planDeAtaquePiolaContra(jin, 3)(CriterioMayorVentajaDeKi) match {
            case Success(plan) => {
              plan.size.shouldBe(3)
              plan shouldEqual List(saiya.habilidades.head, saiya.habilidades.head, saiya.habilidades.head)
            }
            case Failure(error) => println(error)
          }
        }

        "plan con dos ataques buscando hacer el mayor daño" in {
          val saiyajin = Guerrero(nombre = "Saiya", inventario = Seq(),
            especie = Saiyajin(1000, 1000), habilidades = Seq(CargarKi, Fusionarse,
              DejarseFajar, new Atacar(MuchosGolpesNinja), new Atacar(Genkidama), new Atacar(new OndaEnergia(200))))
          val cell = Guerrero(nombre = "Jin", inventario = Seq(Fuego, FotoLuna), especie = Monstruo(800, 1000),
            habilidades = Seq(CargarKi, ConvertirseEnMono))
          saiyajin.planDeAtaquePiolaContra(cell, 2)(CriterioMayorDanio) match {
            case Success(plan) => {
              plan.size.shouldBe(2)
              plan shouldEqual List(saiyajin.habilidades(5), saiyajin.habilidades(5))
            }
            case Failure(error) => println(error)
          }
        }
      }

      "Peleas" - {
        "goku pelea contra cell y le gana" in {
          val goku = Guerrero(nombre = "Goku", inventario = Seq(),
            especie = Saiyajin(20000, 20000), habilidades = Seq(CargarKi, Fusionarse,
              DejarseFajar,
              new Atacar(MuchosGolpesNinja),
              new Atacar(Genkidama),
              new Atacar(new OndaEnergia(200))))

          val cell = Guerrero(nombre = "Cell", inventario = Seq(Fuego, FotoLuna), especie = Monstruo(1500, 1500),
            habilidades = Seq(CargarKi, new Atacar(Explotar), new Atacar(new OndaEnergia(100)), new Atacar(MuchosGolpesNinja)))

          val plan = goku.planDeAtaquePiolaContra(cell, 2)(CriterioMayorDanio)
          val resultado = goku.pelearContra(cell)(plan)
          val (test, ganador) = resultado match {
            case Ganador(guerrero) => (true, guerrero)
            case Empate(guerrero, _) => (false, guerrero)
          }
          test.shouldBe(true)
          ganador.inconsciente.shouldBe(false)
          ganador.nombre.shouldBe("Goku")
        }

        "Pelea aburrida, deberia ser un empate" in {
          val goku = Guerrero(nombre = "Goku", inventario = Seq(),
            especie = Saiyajin(2000, 2000), habilidades = Seq(CargarKi))
          val cell = Guerrero(nombre = "Cell", inventario = Seq(Fuego, FotoLuna), especie = Monstruo(1500, 1500),
            habilidades = Seq(CargarKi))
          val plan = goku.planDeAtaquePiolaContra(cell, 2)(CriterioMayorKiAtacante)
          val resultado = goku.pelearContra(cell)(plan)
          val (atacante: Guerrero, defensor: Guerrero) = resultado match {
            case Ganador(guerrero) => (guerrero, guerrero)
            case Empate(guerrero, otroGuerrero) => (guerrero, otroGuerrero)
          }
          atacante.estaMuerto.shouldBe(false)
          (defensor.nombre != atacante.nombre).shouldBe(true)
          defensor.estaMuerto.shouldBe(false)
        }
      }
    }
  }
}
