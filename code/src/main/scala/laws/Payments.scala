package laws

import Money._

import cats._
import cats.data._
import cats.implicits._

object Payments extends Utils {
  def creditAmount: Payment => Money = { p => if (p.amount.isDebit) zeroMoney else p.amount }

  // generic implementation
  // manipulation logic moved to library code for Money
  // abstraction - hence more reusable
  
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
    // Map[K, V] has a monoid if V has a monoid (comes straight from the library)
    // Note: 2 levels of fusion occurring here based on monoids - Money has a monoid
    // and hence the Map args here mappends. Again each Money has a Map which has a BigDecimal that
    // has a monoid - hence that Map also mappends
    
    implicit val m = Money.MoneyAddMonoid
    currentBalances |+| currentPayments
  }

  // once we have a monoid for a type, we get monoids for free for all functions
  // that has that type as the co-domain. `mappend` for functions is composition
  // which we get for free here since `Money` has a monoid

  def discountedValuation(payments: List[Payment]) = {
    def discount: Payment => Money = { p =>
      if (p.amount.isDebit) zeroMoney
      else Money(p.amount.toBaseCurrency * -0.2, USD)
    }

    implicit val m: Monoid[Money] = Money.MoneyAddMonoid
    mapReduce(payments)(creditAmount |+| discount)
  }
}
