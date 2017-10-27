package concrete.m1

import java.time.OffsetDateTime
import Money._

object Payments {
  def creditAmount(p: Payment): Money = if (p.amount.isDebit) zeroMoney else p.amount

  // concrete implementation
  def valuation(payments: List[Payment]): Money = payments.foldLeft(zeroMoney) { (a, e) =>
    add(a, creditAmount(e))
  }

  // concrete implementation that uses concrete methods of List
  def maxPayment(payments: List[Payment]): Money =
    payments.map(creditAmount).maxBy(_.toBaseCurrency)

  // oops .. complex logic here
  def newBalances(currentBalances: Map[Account, Money], 
    currentPayments: Map[Account, Money]): Map[Account, Money] = {
    // complicated logic that merges the 2 Maps
    Map.empty[Account, Money]
  }

}
