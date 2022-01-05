package org.traditional

import java.util.concurrent.atomic.AtomicLong

object AtomicUID extends  WrapperMain {
  private val uid = new AtomicLong(0L)
  def getUniqueId(): Long = uid.incrementAndGet()
  execute{log (s"Uid asynchronously: ${getUniqueId()}")}
  log(s"Got a unique id : ${getUniqueId()}")
}
