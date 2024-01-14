/*
 * Copyright 2024 circe
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.circe

import cats.kernel.Eq
import cats.kernel.instances.all.*
import cats.syntax.eq.*
import io.circe.testing.CodecTests
import io.circe.tests.CirceMunitSuite
import io.circe.{ Codec, Decoder, Encoder, Json }
import org.scalacheck.{ Arbitrary, Gen }

object RecursiveDerivesSuite {

  case class Inner[A](
    field: A
  )

  object Inner {
    given [A: Encoder]: Encoder[Inner[A]] = Encoder.derived
  }

  case class Outer(
    a: Option[Inner[String]]
  )

  object Outer {
    given Encoder[Outer] = Encoder.derived
  }

}

class RecursiveDerivesSuite extends CirceMunitSuite {
  import RecursiveDerivesSuite.*

  test("Derivation uses pre-existing given codecs") {
    import io.circe.syntax.*

    val foo = Outer(
      a = Some(Inner("c"))
    )
    val expected = Json.obj(
      "a" -> Json.obj(
        "field" -> "c".asJson
      )
    )

    assert(foo.asJson === expected)
  }

}
