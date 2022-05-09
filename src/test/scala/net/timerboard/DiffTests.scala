package net.timerboard

import cats.effect.*
import cats.implicits.*
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*
import net.andimiller.munit.cats.effect.styles.FlatIOSpec

import scala.io.Source

class DiffTests extends FlatIOSpec {

  def loadJsonFile[D: Decoder](name: String): IO[D] = Files[IO]
    .readAll(Path(s"src/test/resources/$name"))
    .through(fs2.text.utf8.decode)
    .compile
    .foldMonoid
    .flatMap(s => IO.fromEither(parse(s)))
    .flatMap(j => IO.fromEither(j.as[D]))

  "Timerboard" should "be able to process WS events" in {
    def attackerScores(m: Model): List[BigDecimal] = m.state.map(_.event.attackers_score)
    for {
      i  <- loadJsonFile[WebsocketEvent]("initial.json")
      d1 <- loadJsonFile[WebsocketEvent]("diff1.json")
      d2 <- loadJsonFile[WebsocketEvent]("diff2.json")
      s1  = Model(BackendSocket.init, List(), List())
      s2  = Timerboard.update(Msg.Payload(i), s1)._1
      _  <- IO(attackerScores(s2)).assertEquals(List("0.4", "0.61").map(BigDecimal(_)))
      s3  = Timerboard.update(Msg.Payload(d1), s2)._1
      _  <- IO(attackerScores(s3)).assertEquals(List("0.4", "0.68").map(BigDecimal(_)))
      s4  = Timerboard.update(Msg.Payload(d2), s3)._1
      _  <- IO(attackerScores(s4)).assertEquals(List("0.4", "0.75").map(BigDecimal(_)))
    } yield ()
  }

}
