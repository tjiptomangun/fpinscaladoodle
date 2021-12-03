package functional_objects

case class Pizza (crustSize: CrustSize, crustType: CrustType, val toppings: Seq[Topping]){

  def addTopping (t: Topping): Pizza = {
    this.copy(toppings = toppings :+ t)
  }

  def removeTopping (t: Topping): Pizza = {
    val newToppings = ListUtils.dropFirstMatch(this.toppings, t)
    this.copy(toppings = newToppings)
  }

  def removeAllToppings(): Pizza = {
    val newTopping = Seq[Topping]();
    this.copy(toppings = newTopping)
  }

  def updateCrustType(crustType: CrustType): Pizza = {
    this.copy(crustType = crustType)
  }

  def updateCrustSize(crustSize: CrustSize): Pizza = {
    this.copy(crustSize = crustSize)
  }

  def getPrice(
              toppingPrices: Map[Topping, Money],
              crustSizePrices: Map[CrustSize, Money],
              crustTypePrices: Map[CrustType, Money]
              ): Money = {
    val base = BigDecimal(10)
    val priceT = this.toppings.foldLeft(BigDecimal(0))((a, x) => {a + toppingPrices(x)})
    base + priceT + crustSizePrices(this.crustSize) + crustTypePrices(this.crustType)
  }

}
