package com.alspizza.pos.services
import com.alspizza.Money
import com.alspizza.pos.model.{CrustSize, CrustType, Order, Topping}

trait AbstractOrderService extends OrderServiceInterface{
  object PizzaService extends PizzaService
  import PizzaService.calculatePizzaPrice

  private lazy val toppingPricesMap = database.getToppingPrices()
  private lazy val crustSizeMap = database.getCrustSizePrices()
  private lazy val crustTypeMap = database.getCrustTypePrices()

  private def calculateOrderPriceInternal(
    o: Order,
    toppingPrices: Map[Topping, Money],
    crustSizePrices: Map[CrustSize, Money],
    crustTypePrices: Map[CrustType, Money]): Money = {

    val pizzaPrices: Seq[Money] = for {
      pizza <- o.pizzas
    } yield {
      calculatePizzaPrice(
        pizza, toppingPrices, crustSizePrices, crustTypePrices
      )
    }

    pizzaPrices.sum

  }

  override def calculateOrderPrice(o: Order): Money =
    calculateOrderPriceInternal(o, toppingPricesMap, crustSizeMap, crustTypeMap)

}
