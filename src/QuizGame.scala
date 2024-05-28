import scala.concurrent.duration._
import scala.util.Random

object QuizGame extends App{

  case class Question(question: String, options: List[String])

  // ----- Easy Questions -----
  val question1 = Question("In which state is the city of Los Angeles in?", List("Nevada", "California", "Florida", "New Mexico"))
  val question2 = Question("Which organism is considered the powerhouse in cells of animals?", List("Mitochondria", "Peroxisome", "Ribosome", "Protein"))
  val question3 = Question("Who was the 1st President of the United States?", List("Abraham Lincoln", "Thomas Jefferson", "George Washington", "John Adams"))
  val question4 = Question("How many inches make a yard? ", List("12 inches", "24 inches", "36 inches", "42 inches"))
  val question5 = Question("Where does the sun rise each morning?", List("North", "South", "East", "West"))
  val easyQuestionSet = List(question1, question2, question3, question4, question5)

  // ----- Medium Questions -----
  val medium_question1 = Question("What is this sequence called: 1, 1, 2, 3, 5, 8, 13...", List("Euler Sequence", "Miletus Sequence", "Guassian Sequence", "Fibonacci Sequence"))
  val medium_question2 = Question("How many bones do humans typically have?", List("182", "206", "215", "221"))
  val medium_question3 = Question("How many planets currently exist in our solar system?", List("7 planets", "8 planets", "9 planets", "10 planets"))
  val medium_question4 = Question("Which country currently has the largest population?", List("United States of America", "India", "China", "Russia"))
  val medium_question5 = Question("Where is the birthplace of the Internet?", List("U.C.L.A in U.S.", "M.I.T in U.S.", "University of Cambridge in UK", "Moscow State University in Russia"))
  val medium_question6 = Question("Who is most credited with inventing the light bulb?", List("Nikola Tesla", "Thomas Edison", "Albert Einstein", "James Madison"))
  val medium_question7 = Question("In which war did the United States not participate in?", List("World War II", "Korean War", "Gulf War", "Boer War"))
  val mediumQuestionSet = List(medium_question1, medium_question2, medium_question3, medium_question4, medium_question5, medium_question6, medium_question7)

  // Mapping of correct answers for sets of questions
  val correctAnswers: Map[Question, Int] = Map.apply(question1 -> 2, question2 -> 1, question3 -> 3, question4 -> 3, question5 -> 3)
  val medium_correctAnswers: Map[Question, Int] = Map.apply(medium_question1 -> 4, medium_question2 -> 2, medium_question3 -> 2, medium_question4 -> 2, medium_question5 -> 1, medium_question6 -> 2, medium_question7 -> 4)

  // Method to establish the game restrictions (timing and set of questions based off difficulty)
  def quizGame(timer: Int, listOfQuestions: List[Question], difficulty: String): Int = {
    println()
    println(s"----- You have $timer seconds to answer ${listOfQuestions.length} questions -----")
    var playerScore = 0
    var currentQuestion = 0
    val timerDeadline = timer.seconds.fromNow
    val randomListOfQuestions = Random.shuffle(listOfQuestions) // Shuffle list of questions for variety
    while (timerDeadline.hasTimeLeft() && currentQuestion < randomListOfQuestions.length) {
            println()
            println(randomListOfQuestions(currentQuestion).question) // Print question and the options
            randomListOfQuestions(currentQuestion).options.foreach(println)
            print("Select your choice (1-4): ")
            val playerChoice = scala.io.StdIn.readInt()
            var answer = correctAnswers.getOrElse(randomListOfQuestions(currentQuestion), None) // default for now***
            if (difficulty == "easy") {
              answer = correctAnswers.getOrElse(randomListOfQuestions(currentQuestion), None)
            }
            else if (difficulty == "medium") {
              answer = medium_correctAnswers.getOrElse(randomListOfQuestions(currentQuestion), None)
            }

            if (playerChoice == answer) {
              println()
              println("Correct!!!")
              playerScore = playerScore + 1
              currentQuestion = currentQuestion + 1
            }
            else {
              println()
              println("Incorrect.")
              currentQuestion = currentQuestion + 1
            }
     }
    if(!(currentQuestion < randomListOfQuestions.length)) {
        println()
        println(s"You have answered all questions under the time limit of $timer seconds!")
      }
    else {
      println()
      println(s"Good try! You almost answered all questions under the time limit of $timer seconds.")
    }
    playerScore // Implicit Return of Player Score
  }

  // Method that obtains the player's preferred difficulty
  def quizSetUp(playerName: String): Unit =
  {
    println()
    println("Please select the difficulty of the Quiz: ")
    println("1) Easy")
    println("2) Medium")
    println("3) Challenging")
    print("Your choice: ")
    val playerDifficulty = scala.io.StdIn.readInt()

    playerDifficulty match {
      case 1 => {
        val finalScore = quizGame(45, easyQuestionSet, "easy")
        if (finalScore == easyQuestionSet.length) {
            println(s"Congrats $playerName, you have answered all questions correctly and achieved $finalScore points in Easy Mode!")
          }
        else {
          println(s"$playerName, you achieved $finalScore points in Easy Mode.")
        }
      }
      case 2 => {
        val finalScore = quizGame(70, mediumQuestionSet, "medium")
        if (finalScore == mediumQuestionSet.length) {
          println(s"Congrats $playerName, you have answered all questions correctly and achieved $finalScore points in Medium Mode!")
        }
        else
          {
            println(s"$playerName, you achieved $finalScore points in Medium Mode.")
          }

      }
      case _ =>
    }
  }

  // ***** Start of the game and obtain player's name *****
  println()
  println(" -----              Welcome to my Quiz Game!               ----- ")
  println(" ----- Answer questions of varying difficult under a timer ----- ")
  print("Enter your name: ")
  val playerName = scala.io.StdIn.readLine()
  quizSetUp(playerName)

}
