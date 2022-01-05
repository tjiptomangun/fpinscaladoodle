package org.main
import scala.collection.mutable

object SyncQueueApp extends WrapperMain {
  class SyncQueue[T] (val n: Int){
    var t : mutable.Queue[T]= mutable.Queue[T]()
    def getWait: T = {
      t.synchronized{
        while(t isEmpty){
          t.wait()
        }
        val tr = t.dequeue()
        t.notify()
        tr

      }
    }

    def putWait(x: T): Unit = {
      t.synchronized{
          while(t.length >= n) {
            t.wait()
          }
          t.enqueue(x)
          t.notify()
      }
    }
  }
  var sv : SyncQueue[Int] = new SyncQueue(5)
  //you synchronized the same object twice
  val producer = thread{
    var i = 0;
    while (i <= 15) {
        println(s"produce  $i")
        sv.putWait(i)
        i += 1
    }
  }

  val consumer  = thread{
    var done = false
    while (!done) {
        val y = sv.getWait
        println(s"consume $y")
        if (y == 15) {
          done = true;
        }
    }
  }
  producer.join()
  consumer.join()
}
