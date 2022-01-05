package org.main

object ThreadCommunicate extends WrapperMain  {

  var result: String = null
  val t = thread{
    result = "\nTitle\n" + "=" * 5
  }
  t.join()
  log(result)

}
