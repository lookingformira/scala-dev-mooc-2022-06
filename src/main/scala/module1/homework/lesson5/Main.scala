package module1.homework.lesson5

object Main extends App {

  val experiment = new BallsExperiment

  val experiments = List.fill(1000000)(experiment)

  val trueExperimentsCount = experiments.map(_.experiment).count(_ == true)
  val goodExperimentsCount = experiments.map(_.goodAnswer).count(_ == true)
  val result = trueExperimentsCount.toDouble / experiments.size.toDouble
  val goodResult = goodExperimentsCount.toDouble / experiments.size.toDouble

  println(result)
  println(goodResult)

}
