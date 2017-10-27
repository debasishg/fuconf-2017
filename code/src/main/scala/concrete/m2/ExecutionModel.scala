package concrete.m2
package trading

trait ExecutionModel {this: RefModel =>
  case class Execution(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal)
}

