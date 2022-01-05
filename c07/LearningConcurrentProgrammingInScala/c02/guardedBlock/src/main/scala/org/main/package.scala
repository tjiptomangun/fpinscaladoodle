package org

package object main {
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

  class WrapperMain extends App with Logging {
    def log(s: String) = println(s)
  }

}
