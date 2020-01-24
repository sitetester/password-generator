import org.scalatest.flatspec.AnyFlatSpec

class PasswordGeneratorSpec extends AnyFlatSpec {
  val minLength = 9

  "It" should "return empty string, if no option is true" in {
    val pass =
      new PasswordGenerator(minLength, false, false, false, false).generate()
    assert(pass.length == 0)
  }

  "It" should "return upperChars string with length = minLength" in {
    val pass =
      new PasswordGenerator(minLength, false, false, false, true).generate()

    assert(pass.length == minLength)
    assert(pass.filter(_.isUpper).length == minLength)
  }

  "It" should "return lowerChars string with length = minLength" in {
    val pass =
      new PasswordGenerator(minLength, false, false, true, false).generate()

    assert(pass.length == minLength)
    assert(pass.filter(_.isLower).length == minLength)
  }

  "It" should "return `number` string with length = minLength" in {
    val pass =
      new PasswordGenerator(minLength, false, true, false, false).generate()

    assert(pass.length == minLength)
    assert(pass.forall(_.isDigit))
  }

  "It" should "return `specialChars` string with length = minLength" in {
    val pass =
      new PasswordGenerator(minLength, true, false, false, false).generate()

    assert(pass.length == minLength)

    assert(pass.forall(!_.isUpper))
    assert(pass.forall(!_.isLower))
    assert(pass.forall(!_.isDigit))
  }

  "It" should "return `specialChars` & `numbers` string with length = minLength/2" in {
    val pass =
      new PasswordGenerator(minLength, true, true, false, false).generate()

    assert(pass.length == minLength)

    assert(pass.filter(_.isLetter).length == 1) // only one added manually at end of generator bcoz of odd minLength
    assert(pass.filter(_.isDigit).length == 4)
    assert(!pass.matches("^[!a-zA-Z]*$"))
  }

  "It" should "return `specialChars`, `numbers` & `lowerChars` string with length = minLength/3" in {
    val pass =
      new PasswordGenerator(minLength, true, true, true, false).generate()

    assert(pass.length == minLength)
    assert(pass.filter(_.isDigit).length == 3)
    assert(pass.filter(_.isLower).length == 3)
    assert(pass.filter(_.isUpper).length == 0)

  }

  "It" should "return `specialChars`, `numbers` , `lowerChars` & `upperChars string with length = minLength/4" in {
    val pass =
      new PasswordGenerator(minLength, true, true, true, true).generate()

    assert(pass.length == minLength)
    assert(pass.filter(_.isDigit).length == 2)
    assert(pass.filter(_.isLower).length == 3) // one added manually bcoz of odd minLength
    assert(pass.filter(_.isUpper).length == 2)
  }

  "It" should "for all check with minLength = 8" in {
    val pass =
      new PasswordGenerator(8, true, true, true, true).generate()

    assert(pass.length == 8)
    assert(pass.filter(_.isDigit).length == 2)
    assert(pass.filter(_.isLower).length == 2) // one added manually bcoz of odd minLength
    assert(pass.filter(_.isUpper).length == 2)
  }

  "It" should "throw exception when minLength < 8" in {
    assertThrows[PasswordGeneratorException] {
      new PasswordGenerator(7, true, true, true, true).generate()
    }
  }

  "It" should "for all check with minLength = 15" in {
    val pass =
      new PasswordGenerator(15, true, true, true, true).generate()

    assert(pass.length == 15)

    assert(pass.filter(_.isDigit).length == 3)
    assert(pass.filter(_.isLower).length == 6)
    assert(pass.filter(_.isUpper).length == 3)
  }

  "It" should "for all check with minLength = 20" in {
    val pass =
      new PasswordGenerator(20, true, true, true, true).generate()

    assert(pass.length == 20)

    assert(pass.filter(_.isDigit).length == 5)
    assert(pass.filter(_.isLower).length == 5)
    assert(pass.filter(_.isUpper).length == 5)
  }

  "It" should "for all check with minLength = 20 when specialChars=true & numbers=true" in {
    val pass =
      new PasswordGenerator(20, true, true, false, false).generate()

    assert(pass.length == 20)

    assert(pass.filter(_.isDigit).length == 10)
    assert(pass.filter(_.isLower).length == 0)
    assert(pass.filter(_.isUpper).length == 0)
  }
}
