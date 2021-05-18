//Using methods as if they are functions
val double = (i: Int) => i *2
double
//double: Int => Int = <function1>

def triple(i: Int) : Int  = i * 3
/* 
triple
<console>:12: error: missing arguments for method triple;
follow this method with `_' if you want to treat it as a partially applied function
triple
^
*/

triple _
