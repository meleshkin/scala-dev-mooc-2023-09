package module3.zio_homework

import module3.zio_homework.timeprint.TimePrinterService.Service
import zio.clock.Clock
import zio.console.Console
import zio.{Has, ULayer, ZIO, ZLayer, clock, console}
import zio.macros.accessible

import java.util.concurrent.TimeUnit

package object timeprint {
  type TimePrinterService = Has[Service]

  @accessible
  object TimePrinterService {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A]
    }

    class ServiceImpl extends Service {
      override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = for {
        start <- clock.currentTime(TimeUnit.SECONDS)
        r <- zio
        end <- clock.currentTime(TimeUnit.SECONDS)
        _ <- console.putStrLn(s"Running time: ${end - start}")
      } yield r
    }

    val live: ULayer[TimePrinterService] = ZLayer.succeed(new ServiceImpl)
  }

}
