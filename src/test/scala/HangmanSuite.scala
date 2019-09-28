import org.scalatest.FunSuite

class HangmanSuite extends FunSuite {

  import HangmanSuite._

  def makeReader(word: String): () => Char = {
    var cs  = word.toList
    () => {
      val char = cs.head
      cs = cs.tail
      char
    }
  }

  test("Reading all symbols from reader") {
    val reader = makeReader("abc")
    val hangman = new Hangman("ABACABA", MaxMistakeCount, SecretLetter, reader)
    hangman.run()

    intercept[NoSuchElementException] {
      reader()
    }
  }

  test("Not reading after success") {
    val reader = makeReader("abcd")
    val hangman = new Hangman("ABACABA", MaxMistakeCount, SecretLetter, reader)
    hangman.run()

    assert(reader() === 'd')
  }

  test(s"Forgiving ${MaxMistakeCount - 1} mistakes") {
    val mistakes = "z" * (MaxMistakeCount - 1)
    val reader = makeReader(mistakes + "abc")
    val hangman = new Hangman("ABACABA", MaxMistakeCount, SecretLetter, reader)
    hangman.run()

    intercept[NoSuchElementException] {
      reader()
    }
  }

  test(s"Not forgiving $MaxMistakeCount mistakes") {
    val mistakes = "z" * MaxMistakeCount
    val reader = makeReader(mistakes + "a")
    val hangman = new Hangman("A", MaxMistakeCount, SecretLetter, reader)
    hangman.run()

    assert(reader() === 'a')
  }

  test("Winning with mistakes") {
    val reader = makeReader("zazzbzc")
    val hangman = new Hangman("ABACABA", MaxMistakeCount, SecretLetter, reader)
    hangman.run()

    intercept[NoSuchElementException] {
      reader()
    }
  }
}

object HangmanSuite {
  val MaxMistakeCount = 5
  val SecretLetter = '*'
}
