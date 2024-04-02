package module3

import module3.zio_homework.config.AppConfig
import module3.zio_homework.timeprint.TimePrinterService
import zio.clock.Clock
import zio.config.ReadError
import zio.console._
import zio.random.{Random, _}
import zio.{IO, Task, UIO, ULayer, URIO, ZIO, clock, random}
import zio.duration.durationInt

import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework extends App {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val int: URIO[Random, Int] = nextIntBetween(1, 3)
  lazy val printGreeting: URIO[Console, Unit] = putStrLn("Please try to guess a number:")
  lazy val printSuccess: URIO[Console, Unit] = putStrLn("Success!")
  lazy val printFailure: URIO[Console, Unit] = putStrLn("Failure...")
  lazy val readLine: Task[String] = ZIO.effect(StdIn.readLine())
  lazy val readInt: Task[Int] = readLine.flatMap(str => ZIO.effect(str.toInt))

  lazy val guessProgram  = for {
    _ <- printGreeting
    userInt <- readInt
    randomInt <- int
    res <- if (userInt == randomInt) printSuccess else printFailure
  } yield res


  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile = ???

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault = {
    val defaultCfg: UIO[AppConfig] = ZIO.effect(AppConfig("localhost", "8443")).orDie
    val cfgEffect: IO[ReadError[String], AppConfig] = config.load.orElse(defaultCfg)
    val result = for {
      cfg <- cfgEffect
      res <- putStrLn(cfg.toString)
    } yield res
    result
  }


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = ZIO.sleep(1 seconds) zipRight random.nextIntBetween(0, 10)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val init: Task[Int] = ZIO.effect(0)
  lazy val sumAsTask: ZIO[Random with Clock, Throwable, Task[Int]] = ZIO.foldLeft(effects)(init)((z, i) => z.flatMap(zz => i.map(ii => ZIO.effect(zz + ii))))
  lazy val sum: ZIO[Random with Clock, Throwable, Int] = sumAsTask.flatMap(aa => aa.map(bb => bb))
  lazy val printSum = sum.flatMap(e => putStrLn(s"Sum of elements in list: ${e.toString}"))

  lazy val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)
  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = for {
    start <- currentTime
    r <- zio
    end <- currentTime
    _ <- putStrLn(s"Running time: ${end - start}")
  } yield r

  lazy val app = printEffectRunningTime(printSum)

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val collected: ZIO[Random with Clock, Nothing, List[Int]] = ZIO.collectAllPar(effects)
  lazy val sum2: ZIO[Random with Clock, Throwable, Int] = collected.flatMap(xx => ZIO.effect(xx.sum))
  lazy val printSum2 = sum2.flatMap(e => putStrLn(s"Sum of elements in list: ${e.toString}"))
  lazy val speedUp = printEffectRunningTime(printSum2)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */

  // package object timeprint


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */
  lazy val printSumEffect = TimePrinterService.printEffectRunningTime(printSum2)
  lazy val env: ULayer[TimePrinterService] = TimePrinterService.live
  lazy val appWithTimeLogg = printSumEffect.provideSomeLayer[Console with Clock with Random](env)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = ZioHomeWorkApp.run(List())

}
