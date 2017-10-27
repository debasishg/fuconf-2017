package patterns.m1

import cats._
import kernel.laws.GroupLaws
import org.scalacheck.{ Arbitrary, Gen, Prop, Properties }
import Prop.forAll
import Arbitrary.arbitrary
import java.time.OffsetDateTime

import Payments._
import scala.math._, BigDecimal._

class PaymentsSpec extends CatsSpec with instances.MoneyInstances { def is = s2"""

  This is a specification for validating Payments

  (Payments) should
     be valuated properly                         $e1 
     be ordered                                   $e2 
     be valuated properly with discounts          $e3 
  """

  import DataGen._

  def e1 = Prop.forAll(Gen.listOfN(10, PaymentGen)) { payments => 
    valuation(payments) != zeroMoney 
  }

  def e2 = Prop.forAll(Gen.listOfN(10, NonZeroPaymentGen) suchThat (_.nonEmpty)) { payments => 
    maxPayment(payments).toBaseCurrency === payments.map(creditAmount(_).toBaseCurrency).max
  }

  def e3 = Prop.forAll(Gen.listOfN(10, NonZeroPaymentGen) suchThat (_.nonEmpty)) { payments => 
    val v = valuation(payments).toBaseCurrency 
    val d = discountedValuation(payments).toBaseCurrency 

    if (v === 0) true
    else (((((v - d)/v)*100).setScale(0, RoundingMode.HALF_UP)) === 20)
  }
}

object DataGen extends Java8Arbitrary {
  implicit lazy val arbCurrency: Arbitrary[Currency] = Arbitrary { Gen.oneOf(AUD, USD, INR, JPY) }

  implicit def moneyArbitrary: Arbitrary[Money] = Arbitrary {
    for {
      i <- Arbitrary.arbitrary[Map[Currency, BigDecimal]]
    } yield new Money(i)
  }

  val genValidAccountNo = Gen.choose(100000, 999999).map(_.toString)
  val genName = Gen.oneOf("john", "david", "mary")

  val validAccountGen = for {
    n <- genValidAccountNo
    m <- genName
    d <- arbitrary[OffsetDateTime]
  } yield Account(n, m, d)

  val PaymentGen = for {
    a <- validAccountGen
    m <- Arbitrary.arbitrary[Money]
    d <- Arbitrary.arbitrary[OffsetDateTime]
  } yield Payment(a, m, d)

  val NonZeroPaymentGen = for {
    a <- validAccountGen
    m <- Arbitrary.arbitrary[Money] suchThat (x => x.items.nonEmpty)
    d <- Arbitrary.arbitrary[OffsetDateTime]
  } yield Payment(a, m, d)
}
