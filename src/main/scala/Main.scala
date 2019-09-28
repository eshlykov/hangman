object Main extends App {
  val random = scala.util.Random
  val dictionary = new Dictionary("dictionary.txt").toList
  val word = dictionary(random.nextInt(dictionary.length))

  new Hangman(word, 5, '*').run()
}
