package catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._

import java.nio.file.Path
import scala.io.{BufferedSource, Source}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]

  def getId(): F[WalletId]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  override def balance: F[BigDecimal] = {
    val path: Path = java.nio.file.Path.of(workDir + id)
    val str: String = java.nio.file.Files.readString(path)
    if (str == null || str.isEmpty) {
      Sync[F].delay(BigDecimal(0))
    } else {
      Sync[F].delay(BigDecimal(str))
    }
  }
  def topup(amount: BigDecimal): F[Unit] = {
    val path: Path = java.nio.file.Path.of(workDir + id)
    val newBalance = balance.map(b => b + amount)
    for {
      bal <- newBalance
      res <- Sync[F].delay(java.nio.file.Files.writeString(path, bal.toString()))
    } yield res
  }
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = {
    for {
      bal <- balance
      write <- topup(amount * -1)
      res <- if (bal < amount) Sync[F].delay(Left(BalanceTooLow)) else Sync[F].delay(Right(write))
    } yield res
  }

  override def getId(): F[WalletId] = Sync[F].delay(id)
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  val workDir = "c:\\code\\wallet\\"
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = {
    Sync[F].delay {
      val path = java.nio.file.Path.of(workDir, id)
      if (java.nio.file.Files.exists(path)) {
        new FileWallet[F](id)
      } else {
        java.nio.file.Files.createFile(path)
        new FileWallet[F](id)
      }
    }
  }

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
