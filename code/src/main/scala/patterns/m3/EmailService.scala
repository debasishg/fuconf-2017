package patterns.m3

// algebra
trait EmailService[M[_]] {
  def sendEmail(paymentCycle: PaymentCycle): M[Unit]
}

