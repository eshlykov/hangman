class Dictionary(path: String) {
  val toList: List[String] = loadDictionary.map(_.toUpperCase)

  private def loadDictionary: List[String] = {
    val wordStream = Option {
      getClass.getResourceAsStream(path)
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
}
