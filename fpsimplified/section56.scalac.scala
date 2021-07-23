//Getting State Working in a for Expression
//scala -Xprint:parse section56.scalac.scala
object section56 {
	case class State[S, A](run: S => (S, A)) {
		def flatMap[B](g: A => State[S, B]): State[S, B] = State { 
			(s0: S) =>  {
				val (s1, a) = run(s0)
				g(a).run(s1)
			}
		}
	
		import State._
		def map[B](f: A=>B): State[S, B] = flatMap(a => State.point(f(a)))
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
}
	
