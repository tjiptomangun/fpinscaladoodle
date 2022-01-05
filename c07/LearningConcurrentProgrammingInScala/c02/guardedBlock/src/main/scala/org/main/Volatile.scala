package org.main

class Page(val txt: String, var position: Int)
object Volatile extends WrapperMain {
  val pages = for (i <- 1 to 20) yield
    new Page("Na" * (100 - 5*i) + "Batman!", -1)
  @volatile var found = false
  //var found = false
  for (p <- pages) yield  thread {
    var i = 0
    while(i < p.txt.length && !found) {
      if (p.txt(i) == '!') {
        p.position = i
        found = true
      }
      else i += 1
    }
  }
  while (!found) {}
  log(s"results: ${pages.map(_.position)}")

}
