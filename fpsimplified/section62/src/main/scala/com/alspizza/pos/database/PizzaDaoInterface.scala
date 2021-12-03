package com.alspizza.pos.database

import com.alspizza.Money
import com.alspizza.pos.model.{CrustSize, CrustType, Topping}

trait PizzaDaoInterface {
  def getToppingPrices(): Map[Topping, Money]
  def getCrustSizePrices(): Map[CrustSize, Money]
  def getCrustTypePrices(): Map[CrustType, Money]
}
