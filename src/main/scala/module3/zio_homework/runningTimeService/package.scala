package module3.zio_homework

import module3.zioConcurrency.currentTime
import zio.{Has, ULayer, URIO, ZIO, ZLayer}
import zio.clock.Clock
import zio.console.{Console, putStrLn}

package object runningTimeService {

  type RunningTimeService = Has[RunningTimeService.Service]

  object RunningTimeService {
    trait Service {
      def printEffectRunningTime[R, E, A](
          zio: ZIO[R, E, A]
      ): ZIO[Console with Clock with R, E, A]
    }

    val live: ULayer[Has[Service]] = ZLayer.succeed(
      new Service {
        override def printEffectRunningTime[R, E, A](
            zio: ZIO[R, E, A]
        ): ZIO[Console with Clock with R, E, A] = for {
          start <- currentTime
          r <- zio
          end <- currentTime
          _ <- putStrLn(s"Running time ${end - start}")
        } yield r
      }
    )

    def printEffectRunningTime[R, E, A](
        zio: ZIO[R, E, A]
    ): ZIO[RunningTimeService with Console with Clock with R, E, A] =
      ZIO.accessM(_.get.printEffectRunningTime(zio))
  }

}
