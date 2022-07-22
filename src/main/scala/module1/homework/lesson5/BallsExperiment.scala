package module1.homework.lesson5

class BallsExperiment {

  private val ballsCollection: List[Int] = List.fill(3)(1) ++ List.fill(3)(0)

  private def getBallFromFive: Int =
    (List.fill(3)(1) ++ List.fill(2)(0)).toArray.apply(scala.util.Random.between(0, 5))

  def experiment: Boolean =
    ballsCollection.toArray.apply(scala.util.Random.between(0, 6)) match {
      case 1 => false
      case 0 => getBallFromFive match {
        case 1 => true
        case 0 => false
      }
    }

  def goodAnswer: Boolean = getBallFromFive match {
    case 1 => true
    case 0 => false
  }
}
