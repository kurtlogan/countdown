package countdown

import scala.io.StdIn
import scala.util.Random
import sys.process._

object Main extends App {

  val smalls = (1 to 10) ++ (1 to 10)
  val large = List(25, 50, 75, 100)

  val numbers = Random.shuffle(large).take(2) ++ Random.shuffle(smalls).take(4)
  val target  = Random.shuffle((100 to 999).toList).head

  println("(pregnant) Rachel seductively walks to the stage")

  println("Your numbers are: ")
  println(numbers.mkString(" "))

  println("Your target is: ")
  println(target)

  // display the list of number to the user
  // ask the user for their first sum
  // calculate the result
  // remove the numbers from the list
  // add the result into the list
  // repeat until success or user resets

  def loop(nums: List[Int]): Unit = {
    println("Your available numbers are:")
    println(nums.mkString(" "))

    val sum = StdIn.readLine()
    val (num1, op, num2) = parse(sum)

    if(nums.contains(num1) && nums.contains(num2)) {
      calculate(num1, num2, op) match {
        case None                   => loop(nums)
        case Some(i) if target == i =>
          println("Congratulations you just won 10 points")
          "curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh" #| "bash" !
        case Some(i)                => loop(i :: nums.filterNot(x => x == num1 || x == num2))
      }
    } else {
      println("Naughty naughty, you have used numbers not in the problem")
      loop(nums)
    }
  }

  def parse(input: String): (Int, Char, Int) = {
    val pattern = "(\\d+)\\s*([+-/*])\\s*(\\d+)".r
    val pattern(num1, op, num2) = input

    (num1.toInt, op.head, num2.toInt)
  }

  def calculate(num1: Int, num2: Int, op: Char): Option[Int] = {
    op match {
      case '+' => Some(num1 + num2)
      case '-' if num1 > num2 => Some(num1 - num2)
      case '/' if num1 % num2 == 0 => Some(num1 / num2)
      case '*' => Some(num1 * num2)
      case _   =>
        println("Invalid operation detected (under the rules of countdown) - beep boop")
        None
    }
  }

  loop(numbers)
}
