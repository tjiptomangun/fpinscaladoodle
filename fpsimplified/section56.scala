//Getting State Working in a for Expression
//:paste section56.scala
case class State[S, A](run: S => (S, A)) {
	def flatMap[B](g: A => State[S, B]): State[S, B] = State { 
		(s0: S) =>  {
			val (s1, a) = run(s0)
			g(a).run(s1)
		}
	}
	def map[B] (fn: A => B): State[S, B] = {
		State (x =>  {
			val (y, a) = run(x)
			(y, fn(a))
		})
	}

	import State._
	def mapx[B](f: A=>B): State[S, B] = flatMap(a => State.point(f(a)))
}

object State {
	def point[S, A](v: A): State[S, A] = State(run = (s) => (s, v));
}

case class GolfState(distance: Int)

def swing(distance: Int): State[GolfState, Int] = State{ (s: GolfState) => {
		val newAmount = s.distance + distance
		(GolfState(newAmount), newAmount)
	} 
}

val stateWithNewDistance: State[GolfState, Int] = for {
	_				<- swing(20)
	_				<- swing(15)
	totalDistance	<- swing(10)
}yield totalDistance

val beginningState = GolfState(0)

val result: (GolfState, Int) = stateWithNewDistance.run(beginningState)

println(s"GolfState:      ${result._1}");
println(s"Total Distance: ${result._2}");

val stateWithNewDistance2: State[GolfState, Int] = swing(20).flatMap((x) => swing(15).flatMap((y) => swing(10).map((z)=>z)))
val result2: (GolfState, Int) = stateWithNewDistance2.run(beginningState)
println(s"GolfState:      ${result2._1}");
println(s"Total Distance: ${result2._2}");
swing(20).map((x) => x + 1).run(beginningState)

case class FibState(x: (Int, Int))
def nextFib: State[FibState, (Int, Int)] = State {
	(s: FibState) => {
		val newVal = s.x._1 + s.x._2
		(FibState(s.x._2, newVal), (s.x._2, newVal))
	}
}

val f0 = FibState((0, 1))

val stateWithFib: State[FibState, (Int, Int)] = for {
	_	<- nextFib
	_	<- nextFib
	_	<- nextFib
	_	<- nextFib
	_	<- nextFib
	lst <- nextFib
}yield lst

val r2 : (FibState, (Int, Int)) = stateWithFib.run(f0); 

/*
val stateWithFibZ: State[FibState, (Int, Int)] = for {
	a	<- nextFib
	b	<- nextFib
	c	<- nextFib
	d	<- nextFib
	e	<- nextFib
	lst <- nextFib
}yield lst


val r3 : (FibState, (Int, Int)) = stateWithFibZ.run(f0); 
*/
val stateWithFibY: State[FibState, (Int, Int)] = for {
	a	<- nextFib
	b	<- nextFib
	c	<- nextFib
	d	<- nextFib
	e	<- nextFib
	lst <- nextFib
}yield a
val r4 = stateWithFibY.run(f0); 



