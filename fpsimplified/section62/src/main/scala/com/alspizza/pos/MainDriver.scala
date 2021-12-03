package com.alspizza.pos

import com.alspizza.pos.model._
import com.alspizza.pos.services.PizzaService

object MainDriver extends  App{

  object PizzaService extends PizzaService

  import PizzaService._

  val address = Address(
    "1 Main Street",
    None,
    "Talketna",
    "AK",
    "99676"
  )

  val customer = Customer (
    "Alvin Alexander",
    "907-555-1212",
    address
  )

  val o1 = Order(
    Seq[Pizza](),
    customer
  )

  val p1 = Pizza(
    MediumCrustSize,
    RegularCrustType,
    Seq(Cheese)
  )

  val newPizzas = o1.pizzas :+ p1;
  val o2 = o1.copy(pizzas = newPizzas)

  val p2 = Pizza(
    MediumCrustSize,
    RegularCrustType,
    Seq(Cheese)
  )

  val p2a = addTopping(p2, Pepperoni)
  val p2b = addTopping(p2a, Mushrooms)
  val p2c = updateCrustType(p2a, ThickCrustType)
  val p2Last = updateCrustSize(p2c, LargeCrustSize)

  val pizzas3 = o2.pizzas :+ p2Last
  val o3 = o2.copy(pizzas = pizzas3)
  println(o3)

  val p2d = updateCrustSize(
    updateCrustType(
      addTopping(
        addTopping(p2, Pepperoni),
        Mushrooms),
      ThickCrustType),
    LargeCrustSize
  )

  import com.alspizza.pos.services.MockDbOrderService.calculateOrderPrice

  val orderPrice = calculateOrderPrice(o3)

  println(s"order Price = $orderPrice")

  val p5 = Pizza(MediumCrustSize, RegularCrustType, Seq(Cheese, Pepperoni, Pepperoni, Sausage))

  val p5a = removeTopping(p5, Pepperoni)

  println("\nSHOULD BE Cheese/Pepperoni/Sausage:")
  println(p5a)



}
