package module1.datacollections.homework

import scala.util.Random

class BallsExperiment {
  def isFirstBlackSecondWhite(): Boolean = {
    val isFirstWhiteBall = doExperiment(create3white3black())
    if (isFirstWhiteBall) false else doExperiment(create3white2black())
  }

  private def doExperiment(balls: List[Int]): Boolean = {
    val rndIndex = Random.nextInt(balls.length)
    val ball = balls(rndIndex)
    isWhite(ball)
  }

  // 1 - white, 0 - black
  private def create3white3black(): List[Int] = 1 :: 1 :: 1 :: 0 :: 0 :: 0 :: Nil
  private def create2white3black(): List[Int] = 1 :: 1 :: 0 :: 0 :: 0 :: Nil
  private def create3white2black(): List[Int] = 1 :: 1 :: 1 :: 0 :: 0 :: Nil
  private def isWhite(i: Int) = i match {
    case 1 => true
    case _ => false
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment)
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite())
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    val analyticalResult: Float = 3f / 10f
    println(s"Experimental result = ${countOfPositiveExperiments / count}, analytical result = ${analyticalResult}")
  }
}

