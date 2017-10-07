package concrete

import scala.math.Ordering

sealed trait Currency
case object USD extends Currency
case object AUD extends Currency
case object JPY extends Currency
case object INR extends Currency

class Money private[concrete] (val items: Map[Currency, BigDecimal]) {
  def toBaseCurrency: BigDecimal = items.foldLeft(BigDecimal(0)) { case (a, (ccy, amount)) =>
    a + Money.exchangeRateWithUSD.get(ccy).getOrElse(BigDecimal(1)) * amount
  }

  def isDebit = toBaseCurrency < 0

  override def toString = items.toList.mkString(",")
}

object Money {
  final val zeroMoney = new Money(Map.empty[Currency, BigDecimal])

  // smart constructor
  def apply(amount: BigDecimal, ccy: Currency) = new Money(Map(ccy -> amount))

  // concrete naive implementation: don't
  def add(m: Money, n: Money) = new Money(
    (m.items.toList ++ n.items.toList)
      .groupBy(_._1)
      .map { case (k, v) => 
        (k, v.map(_._2).sum) 
      }
    )

  final val exchangeRateWithUSD: Map[Currency, BigDecimal] = 
    Map(AUD -> 0.76, JPY -> 0.009, INR -> 0.016, USD -> 1.0)
}
