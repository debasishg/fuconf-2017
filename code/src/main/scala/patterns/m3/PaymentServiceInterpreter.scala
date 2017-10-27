package patterns.m3

import cats._
import cats.data._
import cats.implicits._

import patterns.m1.Money
import Payments._

class PaymentServiceInterpreter[M[_]](implicit me: MonadError[M, Throwable])
  extends PaymentService[M] {

  def paymentCycle: M[PaymentCycle] = PaymentCycle(10, 2014).pure[M]

  def qualifyingAccounts(p: PaymentCycle): M[Vector[Account]] =
    Vector.empty[Account].pure[M]

  def payments(accounts: Vector[Account]): M[Vector[Payment]] = 
    if (accounts.isEmpty) me.raiseError(new IllegalArgumentException("Empty account list"))
    else Vector.empty[Payment].pure[M]

  def adjustTax(payments: Vector[Payment]): M[Vector[Payment]] =
    payments.pure[M]

  def postToLedger(payments: Vector[Payment]): M[Unit] = {
    val amountToPost = valuation(payments)
    //.. do the posting
    ().pure[M]
  }

  private def valuation(payments: Vector[Payment]): Money = ???

}


