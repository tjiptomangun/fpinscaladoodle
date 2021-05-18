//how to write functions that functions as input parameters
def sayHello(callback: () => Unit): Unit = {
	callback();
}

def helloAl(): Unit = {
	println("Hello, Al")
}

sayHello(helloAl);

def holaLorenzo(): Unit = {
	println("Hola, Lorenzo!") 
}

sayHello(holaLorenzo);

