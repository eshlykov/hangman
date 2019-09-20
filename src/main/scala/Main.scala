import java.io.File

object Main extends App {
  val dictionary: List[String] = loadDictionary.map(_.toUpperCase)

  def subFile(file: File, children: String*) = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }

  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, ("src" :: "main" :: "resources" :: resourcePath): _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }

  def loadDictionary: List[String] = {
    val wordStream = Option {
      getClass.getResourceAsStream("dictionary.txt")
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val source = io.Source.fromInputStream(wordStream)
      source.getLines.toList
    } catch {
      case exception: Exception =>
        println(s"Could not load word list: $exception")
        throw exception
    } finally {
      wordStream.close()
    }
  }

  @scala.annotation.tailrec
  def update(xs: List[Char], ys: List[Char], char: Char, acc: List[Char]): List[Char] =
    (xs, ys) match {
      case (`char` :: xs, '*' :: ys) => update(xs, ys, char, char :: acc)
      case (_ :: xs, y :: ys) => update(xs, ys, char, y :: acc)
      case (Nil, Nil) => acc.reverse
      case _ => throw new RuntimeException("Words' length are different")
    }

  @scala.annotation.tailrec
  def game(word: String, acc: String, mistakeCount: Int): Unit = {
    if (mistakeCount == maxMistakeCount)
      println("You lost!")
    else {
      println("Guess a letter:")
      val char = scala.io.StdIn.readChar.toUpper
      if (word.contains(char) && !acc.contains(char)) {
        println("Hit!")
        val accUpdated = update(word.toList, acc.toList, char, List.empty).mkString
        println(s"The word: $accUpdated")
        if (!accUpdated.contains("*"))
          println("You won!")
        else
          game(word, accUpdated, mistakeCount)
      } else {
        println(s"Missed, mistake ${mistakeCount + 1} out of 5.")
        println(s"The word: $acc")
        game(word, acc, mistakeCount + 1)
      }
    }
  }

  val maxMistakeCount = 5

  val random = scala.util.Random
  val word = dictionary(random.nextInt(dictionary.length))
  game(word, "*" * word.length, 0)
}
