import scala.util.Random

class PasswordGenerator(var minLength: Int = 8,
                        var special: Boolean = true,
                        var numbers: Boolean = true,
                        var lower: Boolean = true,
                        var upper: Boolean = true) {

  def generate(): String = {

    if (minLength < 8) {
      throw new PasswordGeneratorException("minLength: 8")
    }

    val numTrueOptions = myArgs.filter(_._2 == true)
    if (numTrueOptions.isEmpty) {
      return ""
    }

    val eachTrueOptionGenerateCount = minLength / numTrueOptions.length
    var randStr: String = ""

    val pg = new PasswordGenerator()
    numTrueOptions.foreach(t => {
      val method = pg.getClass
        .getMethod(t._1.toString, eachTrueOptionGenerateCount.getClass)

      val randChars = method.invoke(pg, eachTrueOptionGenerateCount.toInt)
      randStr += randChars.toString
    })

    if (numTrueOptions.length % 2 == 0) {
      // e.g minLength = 15
      // 15 - (4 * 3) = 2
      randStr += lower(
        minLength - (eachTrueOptionGenerateCount * numTrueOptions.length))
    }

    Random.shuffle(randStr).mkString
  }

  def myArgs: List[(String, AnyRef)] = {
    this.getClass.getDeclaredFields.toList
      .map(i => {
        i.setAccessible(true)
        i.getName -> i.get(this)
      })
  }

  def lower(numTimes: Int): String = {
    val lowerChars = ('a' to 'z').toList
    Random.shuffle(lowerChars).take(numTimes).mkString
  }

  def special(numTimes: Int): String = {
    val specialChars = "!@#$%^&*()_+=-[]{}\\|';:/?.>,<`~".toList
    Random.shuffle(specialChars).take(numTimes).mkString
  }

  def currentMethodName(): String =
    Thread.currentThread.getStackTrace()(2).getMethodName

  def upper(numTimes: Int): String = {
    val upperChars = ('A' to 'Z').toList
    Random.shuffle(upperChars).take(numTimes).mkString
  }

  def numbers(numTimes: Int): String = {
    (1 to numTimes).map(_ => Random.nextInt(9)).mkString
  }
}

class PasswordGeneratorException(message: String) extends Exception(message)
