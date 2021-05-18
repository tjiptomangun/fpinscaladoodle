/*The Great FP Terminology Barrier*/
/** 
 * High order function is function that take other functions
 * as parameter or return function as a result
 * https://docs.scala-lang.org/tour/higher-order-functions.html
*/
def urlBuilder(ssl: Boolean, domainName: String) : (String, String) => String = {
	val schema = if(ssl) "https://" else "http://"
	(endpoint: String, query: String) => s"$schema$domainName/$endpoint?$query"
}
val domain= "www.example.com"
def getURL = urlBuilder(ssl = true, domain)
val endpoint = "users"
val query = "id=1"
val url = getURL(endpoint, query)

