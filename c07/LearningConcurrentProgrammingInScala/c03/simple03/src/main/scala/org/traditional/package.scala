package org

import scala.concurrent.ExecutionContext

package object traditional {
  trait Logging {
    def log(s: String): Unit
    def warn(s: String)= log("WARN: " + s)
    def error(s: String)= log("ERROR: " + s)
  }

  def thread(body: => Unit): Thread = {
    val t = new Thread{
      override def run()= body
    }
    t.start()
    t
  }

  def execute(body: => Unit) = ExecutionContext.global.execute(
    new Runnable {
      def run() = body
    }
  )

  class WrapperMain extends App with Logging {
    def log(s: String) = println(s)
  }
}
