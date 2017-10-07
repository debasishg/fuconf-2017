package concrete

import java.time.OffsetDateTime

case class Account(no: String, name: String, openDate: OffsetDateTime, closeDate: Option[OffsetDateTime] = None) {
  override def equals(o: Any): Boolean = o match {
    case Account(`no`, _, _, _) => true
    case _ => false
  }

  override def hashCode() = no.##
}

case class Payment(account: Account, amount: Money, dateOfPayment: OffsetDateTime)
