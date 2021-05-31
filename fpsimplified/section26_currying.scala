//scalac -Xprint:all section26_currying.scala
// /opt/apps/scala-2.10.2/bin/scalac -Xprint:all section26_currying.scala
class Currying {
	def f1(a: Int, b: Int) = {a + b}
	def f2(a: Int)(b: Int) = {a + b}
}
