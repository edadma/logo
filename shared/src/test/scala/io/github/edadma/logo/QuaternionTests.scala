package io.github.edadma.logo

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class QuaternionTests extends AnyFreeSpec with Matchers with Test:
  // Basic quaternion multiplication identities
  "i squared" in {
    eval("i * i") shouldBe "-1"
  }

  "j squared" in {
    eval("j * j") shouldBe "-1"
  }

  "k squared" in {
    eval("k * k") shouldBe "-1"
  }

  // Hamilton's formula: i*j*k = -1
  "i*j*k" in {
    eval("i * j * k") shouldBe "-1"
  }

  // Quaternion multiplication rules
  "i*j = k" in {
    eval("i * j") shouldBe "k"
  }

  "j*k = i" in {
    eval("j * k") shouldBe "i"
  }

  "k*i = j" in {
    eval("k * i") shouldBe "j"
  }

  // Anti-commutative property
  "j*i = -k" in {
    eval("j * i") shouldBe "-k"
  }

  "k*j = -i" in {
    eval("k * j") shouldBe "-i"
  }

  "i*k = -j" in {
    eval("i * k") shouldBe "-j"
  }

  // Scalar multiplication
  "2*i" in {
    eval("2 * i") shouldBe "2i"
  }

  "i*3" in {
    eval("i * 3") shouldBe "3i"
  }

  // Addition
  "i + j" in {
    eval("i + j") shouldBe "i+j"
  }

  "1 + i" in {
    eval("1 + i") shouldBe "1+i"
  }

  // Complex arithmetic with quaternions
  "2 + 3*i" in {
    eval("2 + 3 * i") shouldBe "2+3i"
  }

  // Full quaternion (spaces required around operators for tokenizer)
  "1 + 2*i + 3*j + 4*k" in {
    eval("1 + 2 * i + 3 * j + 4 * k") shouldBe "1+2i+3j+4k"
  }
