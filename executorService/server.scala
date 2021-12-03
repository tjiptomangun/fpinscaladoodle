//https://twitter.github.io/scala_school/concurrency.html
import java.net.{Socket, ServerSocket}
import java.util.concurrent.{Executors, ExecutorService}
import java.util.Date

class NetworkService(port: Int, poolSize: Int) extends Runnable {
	val serverSocket = new ServerSocket(port)
	val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)

	def run() {
		try {
			while(true) {
				val socket = serverSocket.accept()
				pool.execute(new Handler(socket))
			}
		}
		finally {
			pool.shutdown()
		}
	}

}
class Handler(socket: Socket) extends Runnable {
	def message = (Thread.currentThread.getName() + "\n").getBytes
	def run {
		socket.getOutputStream.write(message)
		socket.getOutputStream.close()
	}
}

(new NetworkService(2020, 2)).run

