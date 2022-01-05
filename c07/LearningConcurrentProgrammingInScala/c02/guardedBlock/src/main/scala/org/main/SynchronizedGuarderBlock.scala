package org.main

//guarded block with notify and wait
//in order to call this methods of an object
//the thread need to own this object monitor
object SynchronizedGuarderBlock extends  WrapperMain {
  val lock = new AnyRef
  var message: Option[String]  = None
  val greeter = thread {
    lock.synchronized{//here lock monitor is acquired by greeter thread

      //occasionally jvm is allowed to wake up a thread that called wait even though
      //there is no corresponding notify call. That is the purpose of `wait` here
      while(message == None) lock.wait()//upon calling wait, lock monitor is released
                                        //and main thread can now acquire lock monitor

      log(message.get)
    }
  }
  lock.synchronized{
    message = Some("Hello !")
    lock.notify()
  }//main thread release lock monitor here

  greeter.join()

}
