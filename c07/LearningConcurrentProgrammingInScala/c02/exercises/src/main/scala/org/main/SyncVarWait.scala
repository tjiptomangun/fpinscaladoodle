package org.main

object SyncVarWait extends WrapperMain {
  class SyncVar[T] {
    var t : T = null.asInstanceOf[T]
    var stateEmpty: Boolean = true
    def get () : T = if (t == null) {
      throw new Exception("No object")
    }
    else {
      stateEmpty = true
      val toRet = t
      t = null.asInstanceOf[T]
      toRet
    }
    def put (x: T): Unit = if (t != null) {
      throw  new Exception("Already exists")
    }
    else {
      t = x
      stateEmpty = false
    }

    def isEmpty  = stateEmpty
    def nonEmpty = !stateEmpty

    def getWait: T = {
      this.synchronized{
        while(t == null){
          this.wait()
        }
        stateEmpty = true;
        val toRet = t;
        t = null.asInstanceOf[T]
        this.notify()
        toRet;
      }
    }

    def putWait(x: T): Unit = {
      this.synchronized{
          while(t != null) {
            this.wait()
          }
          stateEmpty = false
          t = x
          this.notify()
      }
    }
  }
  var sv : SyncVar[Int] = new SyncVar()
  //you synchronized the same object twice
  val producer = thread{
    var i = 0;
    while (i <= 15) {
      if (sv.isEmpty) {
        println(s"produce  $i")
        sv.putWait(i)

        i += 1
      }

    }
  }

  val consumer  = thread{
    var done = false
    while (!done) {
      if (sv.nonEmpty) {
        val y = sv.getWait
        println(s"consume $y")
        if (y == 15) {
          done = true;
        }
      }
    }
  }
  producer.join()
  consumer.join()
}
