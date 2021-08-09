package monads

trait Monad[M[_]] {

  def lift[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]):M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => lift[B](f(a)))

}
