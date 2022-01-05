package org.traditional

import scala.concurrent.ExecutionContext

object ExecutionContextGlobal extends  WrapperMain {
  val ectx = ExecutionContext.global
  ectx.execute(new Runnable {
    override def run(): Unit = log("Running on the execution context")
  })
  Thread.sleep(500)

}
