package monads

case class State [S, A] (run: S => (S, A)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] = State {
    (s0: S) => {
      // create a new (state,value) by applying `run` to the initial state, `s0`.
      // as shown above, `run` transforms an S to an (S,A).
      val (s1, a) = run(s0)

      // create a new State by applying `g` to `a`. as shown above,
      // `g` transforms an A to a `State[S,B]`.
      val s2 = g(a)

      // create a final result by applying s2.run to s1.
      // once again, `run` transforms an S to an (S,A).
      val rez = s2.run(s1)


      // yield this value; it is the param that's passed to State's
      // constructor, so this function yields a State[S,B] as its
      // final result
      rez
    }
  }

  def map[B](f: A => B): State[S, B] = flatMap((a => State.point(f(a))))
}

object State {
  def point[S, A](v: A): State[S, A] = State(run = (s) => (s, v))
}
