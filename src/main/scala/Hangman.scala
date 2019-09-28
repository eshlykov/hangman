class Hangman(dictionaryPath: String, maxMistakeCount: Int, secretLetter: Char) {
  def run(): Unit = {
    val word = chooseWord
    val template = secretLetter.toString * word.length
    game(word, template, 0)
  }

  private def chooseWord: String = {
    val random = scala.util.Random
    val dictionary = new Dictionary(dictionaryPath).toList
    dictionary(random.nextInt(dictionary.length))
  }

  private def game(word: String, template: String, mistakeCount: Int): Unit = {
    def processHit(template: String, char: Char): Unit = {
      printHitMessage()
      val newTemplate = templateUpdates(word, template, char)
      printCurrentTemplate(newTemplate)
      if (isWin(newTemplate))
        printWinMessage()
      else
        game(word, newTemplate, mistakeCount)
    }

    def processMiss(template: String, mistakeCount: Int): Unit = {
      printMissMessage(mistakeCount)
      printCurrentTemplate(template)
      game(word, template, mistakeCount)
    }

    if (mistakeCount == maxMistakeCount)
      printLoseMessage()
    else {
      val char = readChar
      if (isHit(word, template, char)) {
        processHit(template, char)
      } else {
        processMiss(template, mistakeCount + 1)
      }
    }
  }

  private def readChar: Char = {
    printGuessMessage()
    scala.io.StdIn.readChar.toUpper
  }

  private def isHit(word: String, template: String, char: Char): Boolean =
    word.contains(char) && !template.contains(char)

  private def isWin(template: String) = !template.contains(secretLetter)

  private def templateUpdates(word: String, template: String, char: Char): String = {
    @scala.annotation.tailrec
    def helper(xs: List[Char], ys: List[Char], acc: List[Char]): List[Char] =
      (xs, ys) match {
        case (`char` :: xs, `secretLetter` :: ys) => helper(xs, ys, char :: acc)
        case (_ :: xs, y :: ys) => helper(xs, ys, y :: acc)
        case (Nil, Nil) => acc.reverse
        case _ => throw new RuntimeException("Words' lengths are different")
      }

    helper(word.toList, template.toList, Nil).mkString
  }

  private def printLoseMessage(): Unit = println("You lost!")

  private def printWinMessage(): Unit = println("You won!")

  private def printHitMessage(): Unit = println("Hit!")

  private def printMissMessage(mistakeCount: Int): Unit =
    println(s"Missed, mistake $mistakeCount out of $maxMistakeCount.")

  private def printGuessMessage(): Unit = println("Guess a letter:")

  private def printCurrentTemplate(template: String): Unit = println(s"The word: $template")
}
