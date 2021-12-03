package functional_objects

object Driver extends App{

  val toppingPrices = MockPizzaDao.getToppingPrices()
  val crustSizePrices = MockPizzaDao.getCrustSizePrices()
  val crustTypePrices = MockPizzaDao.getCrustTypePrices()

  val  pizza1 = Pizza(MediumCrustSize, ThinCrustType, Seq(Cheese, Pepperoni))

  val pizza2 = pizza1.addTopping(Olives)
  val pizza3 = pizza2.updateCrustSize(LargeCrustSize)

  println(s"pizza3 : $pizza3")


  val pizzaPrice = pizza3.getPrice(toppingPrices, crustSizePrices, crustTypePrices)

  println(s"price of pizza3: $pizzaPrice")

  val pizza4 = pizza1.addTopping(Olives)
    .updateCrustSize(LargeCrustSize)
    .updateCrustType(ThickCrustType)
  println(s"pizza4: $pizza4")


}
