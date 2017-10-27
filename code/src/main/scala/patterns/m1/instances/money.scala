package patterns.m1
package instances

import cats._
import cats.data._
import cats.implicits._

trait MoneyInstances {
  final val zeroMoney = new Money(Map.empty[Currency, BigDecimal])

  val MoneyAddMonoid: Monoid[Money] = new Monoid[Money] {
    def combine(m: Money, n: Money): Money = new Money(m.items |+| n.items)
    def empty: Money = zeroMoney
  }

  implicit val MoneyEq: Eq[Money] = new Eq[Money] {
    def eqv(m: Money, n: Money): Boolean = m.items === n.items
  }

  val MoneyOrder: Order[Money] = new Order[Money] {
    def compare(m: Money, n: Money) = {
      val mbase = m.toBaseCurrency
      val nbase = n.toBaseCurrency
      if (mbase > nbase) 1
      else if (mbase < nbase) -1
      else 0
    }
  }

  val MoneyOrderMonoid: Monoid[Money] = new Monoid[Money] {
    def combine(m: Money, n: Money): Money = 
      if (m === zeroMoney) n
      else if (n === zeroMoney) m
      else if (MoneyOrder.compare(m, n) >= 0) m else n

    def empty: Money = zeroMoney
  }
}

