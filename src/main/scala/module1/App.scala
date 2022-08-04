package module1

import scala.util.{Failure, Success, Try}

object App {
  def main(args: Array[String]): Unit = {
    println(s"Hello from " +
      s"${Thread.currentThread().getName}")

//    val t1 = new Thread{
//      override def run(): Unit = {
//        Thread.sleep(1000)
//        println(s"Hello from " +
//          s"${Thread.currentThread().getName}")
//      }
//    }
//    val t2 = new threads.Thread1
//    t1.start()
//    t1.join()
//    t2.start()



//    def rates = {
//      val t1 = threads.getRatesLocation5
//      val t2 = threads.getRatesLocation6
//      t1.onComplete{ i1 =>
//        t2.onComplete{ i2 =>
//          println(s"Result: ${i1 + i2}")
//        }
//      }
//
//      val r: threads.ToyFuture[Int] = for{
//        i1 <- t1
//        i2 <- t2
//      } yield  i1 + i2
//
//      r.onComplete(println(_))
//    }
//
//    threads.printRunningTime(rates)

//    tryObj.readFromFile2() match {
//      case Failure(exception) => println(exception.getMessage)
//      case Success(value) => println(value)
//    }
    import scala.concurrent.ExecutionContext.Implicits.global

//    def rates = {
//      val f1 = future.getRatesLocation1
//      val f2 = future.getRatesLocation2
//      for{
//        v1 <- f1
//        v2 <- f2
//      } yield println(v1 + v2)
//    }
//
//    future.printRunningTime(future.getRatesLocation1 zip future.getRatesLocation2)

   // future.f3

    println(promise.p1.isCompleted)
    println(promise.f1.isCompleted)
    promise.p1.complete(Try(10))
    println(promise.p1.isCompleted)
    println(promise.f1.isCompleted)
    promise.f1.foreach(println)

    Thread.sleep(4000)
  }
}
