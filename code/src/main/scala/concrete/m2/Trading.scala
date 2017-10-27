package concrete.m2
package trading

import cats._
import cats.data._
import cats.implicits._


trait Trading[Account, Trade, ClientOrder, Order, Execution, Market] {

  def clientOrders(c: Config, orders: List[ClientOrder]): List[Order]
  def execute(c: Config, market: Market, brokerAccount: Account, order: Order): List[Execution]
  def allocate(c: Config, accounts: List[Account], execution: Execution): List[Trade]

  def tradeGeneration(
    config: Config,
    orders: List[ClientOrder],
    market: Market, 
    broker: Account, 
    clientAccounts: List[Account]) = {

    for {
      o <- clientOrders(config, orders)
      e <- execute(config, market, broker, o)
      t <- allocate(config, clientAccounts, e)
    } yield t
  }
}
