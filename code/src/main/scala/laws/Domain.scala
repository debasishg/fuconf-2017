package laws

import java.time.OffsetDateTime

case class Account(no: String, name: String, openDate: OffsetDateTime, closeDate: Option[OffsetDateTime] = None)
case class Payment(account: Account, amount: Money, dateOfPayment: OffsetDateTime)

