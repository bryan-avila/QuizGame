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
  val medium_question1 = Question("What is this sequence called: 1, 1, 2, 3, 5, 8, 13...", List("Euler Sequence", "Miletus Sequence", "Gaussian Sequence", "Fibonacci Sequence"))
  val medium_question2 = Question("How many bones do humans typically have?", List("182", "206", "215", "221"))
  val medium_question3 = Question("How many planets currently exist in our solar system?", List("7 planets", "8 planets", "9 planets", "10 planets"))
  val medium_question4 = Question("Which country currently has the largest population?", List("United States of America", "India", "China", "Russia"))
  val medium_question5 = Question("Where is the birthplace of the Internet?", List("U.C.L.A in U.S.", "M.I.T in U.S.", "University of Cambridge in UK", "Moscow State University in Russia"))
  val medium_question6 = Question("Who is most credited with inventing the light bulb?", List("Nikola Tesla", "Thomas Edison", "Albert Einstein", "James Madison"))
  val medium_question7 = Question("In which war did the United States not participate in?", List("World War II", "Korean War", "Gulf War", "Boer War"))
  val mediumQuestionSet = List(medium_question1, medium_question2, medium_question3, medium_question4, medium_question5, medium_question6, medium_question7)

  // ----- Hard Questions -----
  val hard_question1 = Question("What is the longest river in the world?", List("Mississippi", "Amazon", "Guangxi", "Nile"))
  val hard_question2 = Question("What is PI rounded to 4 decimal places?", List("3.1345", "3.1439", "3.1416", "3.1426"))
  val hard_question3 = Question("Who wrote To Kill A Mockingbird?", List("Sylvia Plath", "Harper Lee", "Kurt Vonnegut", "J.D. Salinger"))
  val hard_question4 = Question("Where did the Industrial Revolution begin?", List("United States", "Britain", "China", "Japan"))
  val hard_question5 = Question("Which is the first film to incorporate sound?", List("Don Juan", "The Wizard Of Oz", "The Jazz Singer", "The Crowd"))
  val hard_question6 = Question("What is the chemical symbol for iron?", List("IR", "OE", "RD", "FE"))
  val hard_question7 = Question("What car was the first to be mass produced?", List("Chevrolet Classic Six", "Ford Model T", "Studebaker 30", "Cadillac 30"))
  val hard_question8 = Question("Which state is directly above Oregon?", List("Idaho", "Washington", "Minnesota", "South Dakota"))
  val hard_question9 = Question("When was the Java programming language founded?", List("2003", "1975", "1987", "1995"))
  val hard_question10 = Question("How are protons charged electrically?", List("Negatively", "Positively", "Both", "Neither"))
  val hardQuestionSet = List(hard_question1, hard_question2, hard_question3, hard_question4, hard_question5, hard_question6, hard_question7, hard_question8, hard_question9, hard_question10)

  // Mapping of correct answers for each question
  val correctAnswers: Map[Question, Int] = Map.apply(question1 -> 2, question2 -> 1, question3 -> 3, question4 -> 3, question5 -> 3)
  val medium_correctAnswers: Map[Question, Int] = Map.apply(medium_question1 -> 4, medium_question2 -> 2, medium_question3 -> 2, medium_question4 -> 2, medium_question5 -> 1, medium_question6 -> 2, medium_question7 -> 4)
  val hard_correctAnswers: Map[Question, Int] = Map.apply(hard_question1 -> 4, hard_question2 -> 3, hard_question3 -> 2, hard_question4 -> 2, hard_question5 -> 1, hard_question6 -> 4, hard_question7 -> 2, hard_question8 -> 2, hard_question9 -> 4, hard_question10 -> 2)

  // Method to establish the game restrictions and present the Quiz questions
  def quizGame(timer: Int, listOfQuestions: List[Question], difficulty: String): Int = {
    println()
    println(s"----- You have $timer seconds to answer ${listOfQuestions.length} questions -----")

    // Initialize score for this Mode and the question to start with
    var playerScore = 0
    var currentQuestion = 0
    val timerDeadline = timer.seconds.fromNow // Grab the amount of time to do Quiz

    val randomListOfQuestions = Random.shuffle(listOfQuestions) // Shuffle list of questions for variety

    // START OF WHILE LOOP: Display questions to the user
    while (timerDeadline.hasTimeLeft() && currentQuestion < randomListOfQuestions.length) {
            println()
            println(randomListOfQuestions(currentQuestion).question) // Print question
            randomListOfQuestions(currentQuestion).options.foreach(println) // Print each question's options
            print("Select your choice (1-4): ")
            val playerChoice = scala.io.StdIn.readInt()

            var answer = correctAnswers.getOrElse(randomListOfQuestions(currentQuestion), None) // Default Choice.
            if (difficulty == "easy") {
              answer = correctAnswers.getOrElse(randomListOfQuestions(currentQuestion), None)
            }
            else if (difficulty == "medium") {
              answer = medium_correctAnswers.getOrElse(randomListOfQuestions(currentQuestion), None)
            }
            else if(difficulty == "hard"){
              answer = hard_correctAnswers.getOrElse(randomListOfQuestions(currentQuestion), None)
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
    // END OF WHILE LOOP: Out of loop due to either running out of time or answering all questions
    if(!(currentQuestion < randomListOfQuestions.length) && timerDeadline.hasTimeLeft()) { // All Q's answered AND time left over?
        println()
        println(s"You answered all ${randomListOfQuestions.length} questions under $timer seconds")
      }
    else if(!timerDeadline.hasTimeLeft() && !(currentQuestion < randomListOfQuestions.length)) { // All Q's answered and no time left?
      println()
      println(s"You answered all ${randomListOfQuestions.length} questions but ran out of time!")
    }
    else if(!timerDeadline.hasTimeLeft() && currentQuestion < randomListOfQuestions.length) { // Some Q's answered AND no time left?
      println()
      println(s"You answered $currentQuestion questions and ran out of time. Try again!")
    }

    playerScore // Implicit Return of Player Score

  }

  // Method that obtains the player's preferred difficulty. Allow players to attempt all three modes in one run
  // Validate that a user cannot re-attempt the same difficulty mode in one run
  // Display the user's final overall score
  def quizSetUp(playerName: String, passedTotalScore: Int, beatEasy: Boolean, beatMedium: Boolean, beatHard: Boolean): Unit =
  {
      println()
      println("Please select the difficulty of the Quiz: ")
      println("1) Easy")
      println("2) Medium")
      println("3) Hard")
      print("Your choice: ")
      val playerDifficulty = scala.io.StdIn.readInt()

      playerDifficulty match {
        // CASE 1: EASY START
        case 1 =>
         if (beatEasy) { // Have they played Easy already?
            println()
            println("***** You've already attempted Easy Mode! Select another! ***** ")
            quizSetUp(playerName, passedTotalScore, beatEasy = true, beatMedium = beatMedium, beatHard = beatHard)
          }

         val modeScore = quizGame(45, easyQuestionSet, "easy") // Start the game on Easy

         if (modeScore == easyQuestionSet.length) { // Have they answered every question correctly?
            println()
            println(s"Congrats $playerName, you have answered all questions correctly and achieved $modeScore points in Easy Mode!")
         }
         else println(s"$playerName, you achieved $modeScore points in Easy Mode.") // Else, show their points

         val updatedScore = modeScore + passedTotalScore

         println(s"Your TOTAL SCORE is $updatedScore")
         println("Would you like to try again? (Y/n)") // Will they play the other modes?
         val userRepeat = io.StdIn.readChar()
         if(userRepeat.toUpper == 'Y') {
           quizSetUp(playerName, passedTotalScore = updatedScore, beatEasy = true, beatMedium = beatMedium, beatHard = beatHard)
         }
         else println(s"----- Thanks for playing $playerName! Your final TOTAL SCORE is $updatedScore")
        // CASE 1: EASY END

        // CASE 2: MEDIUM START
       case 2 =>
         if (beatMedium) { // Have they played Medium already?
            println()
            println("***** You've already attempted Medium Mode! Select another! ***** ")
            quizSetUp(playerName, passedTotalScore, beatEasy =  beatEasy, beatMedium = true, beatHard = beatHard)
         }

         val modeScore = quizGame(70, mediumQuestionSet, "medium") // Start the game on Medium

         if (modeScore == mediumQuestionSet.length) { // Have they answered all questions correctly?
           println()
           println(s"Congrats $playerName, you have answered all questions correctly and achieved $modeScore points in Medium Mode!")
         }
         else println(s"$playerName, you achieved $modeScore points in Medium Mode.") // Else, show their points

         val updatedScore = modeScore + passedTotalScore

         println(s"Your TOTAL SCORE is $updatedScore")
         println("Would you like to try again? (Y/n)") // Will they play other modes?
         val userRepeat = io.StdIn.readChar()
         if(userRepeat.toUpper == 'Y') {
           quizSetUp(playerName, passedTotalScore = updatedScore, beatEasy = beatEasy, beatMedium = true, beatHard = beatHard)
         }
         else println(s"----- Thanks for playing $playerName! Your final TOTAL SCORE is $updatedScore")
       // CASE 2: MEDIUM END

       // CASE 3: HARD START
       case 3 =>
         if (beatHard){ // Have they played Hard already?
           println()
           println("***** You've already attempted Hard Mode! Select another! *****")
            quizSetUp(playerName, passedTotalScore, beatEasy =  beatEasy, beatMedium = beatMedium, beatHard = true)
         }

         val modeScore = quizGame(timer = 90, hardQuestionSet, "hard") // Start the game on hard

         if(modeScore == hardQuestionSet.length) { // Answered all questions correctly?
            println()
            println(s"Congrats $playerName, you have answered all questions correctly and achieved $modeScore points in Hard Mode!")
          }
         else {
          println()
          println(s"$playerName, you achieved $modeScore points in Hard Mode.") // Else, show their points
        }

         val updatedScore = modeScore + passedTotalScore

         println(s"Your TOTAL SCORE is $updatedScore")
         if (beatEasy && beatMedium) return
         println("Would you like to try again? (Y/n)") // Will they play other modes?
         val userRepeat = io.StdIn.readChar()
         if(userRepeat.toUpper == 'Y') {
           quizSetUp(playerName, passedTotalScore = updatedScore, beatEasy = beatEasy, beatMedium = beatMedium, beatHard = true)
         }
         else println(s"----- Thanks for playing $playerName! Your final TOTAL SCORE is $updatedScore")
        // CASE 3: HARD END
      }
  }

  // ***** Start of the game and obtain player's name *****
  // ***** Start of the game and obtain player's name *****
  // ***** Start of the game and obtain player's name *****
  println()
  println(" -----                Welcome to my Quiz Game!                   ----- ")
  println(" ----- Answer questions of varying difficulty under a time limit ----- ")
  print("Enter your name: ")
  val playerName = scala.io.StdIn.readLine()
  quizSetUp(playerName, passedTotalScore = 0, beatEasy = false, beatMedium = false, beatHard = false) // Each time the program is ran, the player starts with 0 score in each mode
}
