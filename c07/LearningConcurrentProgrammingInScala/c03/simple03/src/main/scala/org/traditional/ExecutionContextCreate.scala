package org.traditional

import java.util.concurrent.ForkJoinPool

import scala.concurrent.ExecutionContext

object ExecutionContextCreate extends WrapperMain {
  val pool = new ForkJoinPool(2)
  val ectx = ExecutionContext.fromExecutorService(pool)
  ectx.execute( new Runnable {
    override def run(): Unit = log(s"running in execution context again")
  })
}
