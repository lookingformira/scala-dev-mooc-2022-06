package module3

import module3.zio_homework.config.load
import module3.zio_homework.runningTimeService.RunningTimeService
import zio.{Has, IO, Schedule, Task, UIO, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.config.ReadError
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

    def loop(number: Int): ZIO[Console, IOException, Unit] =
      putStrLn("write number from 1 to 3") *> getStrLn.flatMap { choice =>
        if (choice.toInt == number) putStrLn(s"Bingo! Answer is $number") else putStrLn("false") *> loop(number)
      }



  lazy val guessProgram: ZIO[Console with Random, IOException, Unit] = for {
    randomNumber <- nextIntBetween(1, 4)
    _ <- loop(randomNumber)
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(condition: A): ZIO[R, E, Unit] = effect.map {
    value => if (value != condition) doWhile(effect)(condition) else ()
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: IO[ReadError[String], config.AppConfig] = load


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Random with Clock, Int] = nextIntBounded(11).delay(1 second)


  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: URIO[Random with Clock, List[Int]] = ZIO.collectAll(List.fill(10)(eff))


  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  val sumEff: URIO[Console with Random with Clock, Int] = for {
    list <- effects
    sum = list.sum
    _ <- putStrLn(s"$sum")
  } yield sum

  lazy val app: URIO[Console with Clock with Random, Int] = zioConcurrency.printEffectRunningTime(sumEff)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  val sumEffSpeedUp: URIO[Console with Random with Clock, Int] = for {
    sum <- ZIO.reduceAllPar(eff, List.fill(10)(eff))(_ + _)
    _ <- putStrLn(s"$sum")
  } yield sum

  lazy val appSpeedUp: URIO[Console with Random with Clock, Int] = zioConcurrency.printEffectRunningTime(sumEffSpeedUp)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[RunningTimeService with Console with Clock with Random, Nothing, Int] = RunningTimeService.printEffectRunningTime(sumEff)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */


  lazy val runApp = appWithTimeLogg.provideSomeLayer[Random with Clock with Console](RunningTimeService.live)
  
}
