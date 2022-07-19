package net.timerboard

import cats.effect.*
import cats.implicits.*
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*
import net.andimiller.munit.cats.effect.styles.FlatIOSpec

import java.time.*
import scala.io.Source

class TimeDiffTests extends FlatIOSpec {

  val now = ZonedDateTime.of(2022, 5, 16, 17, 21, 5, 90, ZoneOffset.UTC)

  "Timerboard" should "be able to diff times" in {
    IO { TimeDiff(now, now.plusMinutes(10)) }.assertEquals("0d 0h 10m 0s")
  }
  "Timerboard" should "be able to diff complex times" in {
    IO { TimeDiff(now, now.plusDays(5).plusHours(4).plusMinutes(2).plusSeconds(62)) }.assertEquals("5d 4h 3m 2s")
  }
  "Timerboard" should "be able to diff negative times" in {
    IO { TimeDiff(now.plusDays(5).plusHours(4).plusMinutes(2).plusSeconds(62), now) }.assertEquals("-5d 4h 3m 2s")
  }


}
