package patterns.m3

import java.time.OffsetDateTime
import patterns.m1.Money
import Money._
import patterns.m1.instances.MoneyInstances

case class Account(no: String, name: String, emailAddress: String, openDate: OffsetDateTime, 
  closeDate: Option[OffsetDateTime] = None)

case class Payment(account: Account, amount: Money, dateOfPayment: OffsetDateTime)

object Payments extends MoneyInstances {
  def creditsOnly(p: Payment): Money = if (p.amount.isDebit) zeroMoney else p.amount
}

case class PaymentCycle(month: Int, year: Int)

