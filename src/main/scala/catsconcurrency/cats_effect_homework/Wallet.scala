package catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._


import java.nio.file.{Path, Paths}
import scala.util.Try

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

  private def getFileWallet:Option[Path] = Try{
    if(!Paths.get(id).toFile.exists()){
      val path =Paths.get(id)
      path.toFile.createNewFile()
      java.nio.file.Files.write(path, "0".getBytes("utf-8"))
    }
    Paths.get(id)
  }.toOption
  private def readBalance:BigDecimal = getFileWallet.map(f=> BigDecimal(java.nio.file.Files.readString(f))).getOrElse(BigDecimal.valueOf(0))
  private def writeBalance(newAmount:BigDecimal):Unit =
    getFileWallet.foreach(path=> {java.nio.file.Files.write(path, (newAmount).toString().getBytes("utf-8"));()})

  def balance: F[BigDecimal] = Sync[F].delay(readBalance)
  def topup(amount: BigDecimal): F[Unit] = Sync[F].delay(writeBalance(readBalance + amount))
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = {
    balance.map(_ - amount).map {
      case res if res <0 => Left(BalanceTooLow)
      case res => Right(writeBalance(res))
    }
  }
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] =  Sync[F].delay(new FileWallet[F](id))

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
  case object UnsupportOperationFS extends WalletError
}
