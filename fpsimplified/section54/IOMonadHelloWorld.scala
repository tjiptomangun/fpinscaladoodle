import cats.effect.IO

object IOMonadHelloWorld  extends  App {
  val helloEffect = IO { println("Hello, world")}
  helloEffect.unsafeRunSync()
}