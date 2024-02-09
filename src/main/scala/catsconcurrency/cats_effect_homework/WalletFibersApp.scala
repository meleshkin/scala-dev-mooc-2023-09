package catsconcurrency.cats_effect_homework

import cats.effect.{FiberIO, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def topupLoop(wallet: Wallet[IO], amount: BigDecimal, sleep: Duration): IO[Unit] = {
    IO.sleep(sleep) *>
    wallet.getId().flatMap(id => IO(println(s"[${Thread.currentThread().getName}]} topup wallet $id"))) *>
    wallet.topup(amount).flatMap(_ => topupLoop(wallet, amount, sleep))
  }
  def balanceLoop(w1: Wallet[IO], w2: Wallet[IO], w3: Wallet[IO]): IO[Unit] = {
    val balance: IO[Unit] = w1.balance.flatMap(b => IO(println(s"[${Thread.currentThread().getName}] Balance of wallet1: $b"))) *>
        w2.balance.flatMap(b => IO(println(s"[${Thread.currentThread().getName}] Balance of wallet2: $b"))) *>
        w3.balance.flatMap(b => IO(println(s"[${Thread.currentThread().getName}] Balance of wallet3: $b")))
    IO.sleep(1 second) *> balance.flatMap(_ => balanceLoop(w1, w2, w3))
  }

  def run: IO[Unit] = for {
    _ <- IO.println("Press enter to stop...")
    wallet1 <- Wallet.fileWallet[IO]("1")
    wallet2 <- Wallet.fileWallet[IO]("2")
    wallet3 <- Wallet.fileWallet[IO]("3")
    // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
    f1 <- topupLoop(wallet1, 100, 1000 millisecond).start
    f2 <- topupLoop(wallet2, 100, 2000 millisecond).start
    f3 <- topupLoop(wallet3, 100, 3000 millisecond).start
    _ <- balanceLoop(wallet1, wallet2, wallet3).start
    _ <- IO.readLine *> f1.cancel *> f2.cancel *> f3.cancel
    _ <- f1.join
  } yield ()
}
