package laws

import Money._

import cats._
import cats.data._
import cats.implicits._

object Payments extends Utils {
  def creditAmount(p: Payment): Money = if (p.amount.isDebit) zeroMoney else p.amount

  // generic implementation
  // manipulation logic moved to library code for Money
  // abstrcation - hence more reusable
  
  def valuation(payments: List[Payment]): Money = {
    implicit val m: Monoid[Money] = Money.MoneyAddMonoid
    mapReduce(payments)(creditAmount)
  }

  def maxPayment(payments: List[Payment]): Money = {
    implicit val m: Monoid[Money] = Money.MoneyOrderMonoid
    mapReduce(payments)(creditAmount)
  }

  def newBalances(currentBalances: Map[Account, Money], 
    currentPayments: Map[Account, Money]): Map[Account, Money] = {

    // fuse the Maps based on the addition monoid
    // Note: 2 levels of fusion occurring here based on monoids - Money has a monoid
    // and hence the Map args here mappends. Again each Money has a Map which has a BigDecimal that
    // has a monoid - hence that Map also mappends
    implicit val m = Money.MoneyAddMonoid
    currentBalances |+| currentPayments
  }
}
