package module3.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._
import cats.data.NonEmptyList

import java.nio.file.Path
import java.util
import scala.collection.mutable

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
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

  import collection.JavaConverters._

  private val strPath = s"$id.txt"

//  private def existCondition(id: WalletId): F[Boolean] = for {
//    path <- Sync[F].pure(java.nio.file.Paths.get(strPath))
//    exist <- Sync[F].delay(java.nio.file.Files.exists(path))
//  } yield exist
  def balance: F[BigDecimal] = for {
    path <- Sync[F].pure(java.nio.file.Paths.get(strPath))
    exist <- Sync[F].delay(java.nio.file.Files.exists(path))
    seq = java.nio.file.Files.readAllLines(path).asScala
    balanceValue <-
      if (exist) Sync[F].pure {
        seq.headOption.map(value => BigDecimal.valueOf(value.toDouble)) match {
          case Some(value) => value
          case None        => BigDecimal.valueOf(0)
        }
      }
      else Sync[F].raiseError(new Exception(s"Wallet does not exist"))
  } yield balanceValue

  private def createWallet: F[BigDecimal] = for {
    path <- Sync[F].pure(java.nio.file.Paths.get(strPath))
    _ <- Sync[F].delay(java.nio.file.Files.createFile(path))
  } yield BigDecimal.valueOf(0)
  def topup(amount: BigDecimal): F[Unit] = for {
    balanceValue <- balance.orElse(createWallet)
    _ <- Sync[F].delay(
      java.nio.file.Files.write(
        java.nio.file.Paths.get(strPath),
        (amount + balanceValue).toString().getBytes
      )
    )
  } yield ()
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    balanceValue <- balance.orElse(createWallet)
    either <- Sync[F].delay {
      if (balanceValue < amount) BalanceTooLow.asLeft[Unit]
      else {
        java.nio.file.Files.write(
          java.nio.file.Paths.get(strPath),
          (balanceValue - amount).toString().getBytes
        )
        ()
      }.asRight[WalletError]
    }
  } yield either
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] =
    Sync[F].delay(new FileWallet[F](id))

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
