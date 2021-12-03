package com.alspizza.pos.services

import com.alspizza.Money
import com.alspizza.pos.database.PizzaDaoInterface
import com.alspizza.pos.model.Order

trait OrderServiceInterface {
  protected def database: PizzaDaoInterface

  def calculateOrderPrice(o: Order): Money

}
