package one.mir.matcher.market

import akka.actor.{Actor, Props}
import one.mir.matcher.MatcherSettings
import one.mir.matcher.api.BatchCancel
import one.mir.matcher.market.OrderHistoryActor._
import one.mir.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import one.mir.matcher.model._
import one.mir.metrics.TimerExt
import one.mir.state.ByteStr
import one.mir.utils.ScorexLogging
import kamon.Kamon
import org.iq80.leveldb.DB

class OrderHistoryActor(db: DB, settings: MatcherSettings) extends Actor with ScorexLogging {

  val orderHistory = new OrderHistory(db, settings)

  private val timer          = Kamon.timer("matcher.order-history")
  private val addedTimer     = timer.refine("event" -> "added")
  private val executedTimer  = timer.refine("event" -> "executed")
  private val cancelledTimer = timer.refine("event" -> "cancelled")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[OrderAdded])
    context.system.eventStream.subscribe(self, classOf[OrderExecuted])
    context.system.eventStream.subscribe(self, classOf[OrderCanceled])
  }

  override def receive: Receive = {
    case ev: OrderAdded =>
      log.debug(s"OrderAdded(${ev.order.order.idStr()}, amount=${ev.order.amount})")
      addedTimer.measure(orderHistory.process(ev))
    case ev: OrderExecuted =>
      log.debug(s"OrderExecuted(s=${ev.submitted.order.idStr()}, c=${ev.counter.order.idStr()}, amount=${ev.executedAmount})")
      executedTimer.measure(orderHistory.process(ev))
    case ev: OrderCanceled =>
      log.debug(s"OrderCanceled(${ev.limitOrder.order.idStr()}, system=${ev.unmatchable})")
      cancelledTimer.measure(orderHistory.process(ev))
    case ForceCancelOrderFromHistory(id) =>
      log.debug(s"ForceCancelOrderFromHistory($id)")
      forceCancelOrder(id)
    case BatchCancel(address, _, ts) =>
      sender() ! orderHistory.updateTimestamp(address, ts)
  }

  def forceCancelOrder(id: ByteStr): Unit = {
    val maybeOrder = orderHistory.order(id)
    for (o <- maybeOrder) {
      val oi = orderHistory.orderInfo(id)
      orderHistory.process(OrderCanceled(LimitOrder.limitOrder(oi.remaining, oi.remainingFee, o), unmatchable = false))
    }
    sender ! maybeOrder
  }

  override def postStop(): Unit = {
    log.info("Stopped OrderHistoryActor")
    super.postStop()
  }
}

object OrderHistoryActor {
  def name: String = "OrderHistory"

  def props(db: DB, settings: MatcherSettings): Props = Props(new OrderHistoryActor(db, settings))

  case class ForceCancelOrderFromHistory(orderId: ByteStr)
}
