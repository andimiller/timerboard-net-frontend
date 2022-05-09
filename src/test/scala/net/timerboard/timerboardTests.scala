package net.timerboard

import cats.implicits.*
import io.circe.*
import io.circe.syntax.*

class timerboardTests extends munit.FunSuite {

  test("dummy test") {
    assert(1 == 1)
  }

  enum AorB:
    case A(a: String)
    case B(b: String)

  test("encode an enum") {}

}
