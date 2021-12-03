def sum(ints: IndexedSeq[Int]): Int = {
	if (ints.size <= 1)
		ints.headOption getOrElse 0
	else {
		val (l, r) = ints.splitAt(ints.length/2)
		val sumL: Par[Int] = Par.unit(sum(l))
		val sumR: Par[Int] = Par.unit(sum(r))
		Par.get(sumL) + Par.get(sumR);
	}
}
//should `unit` begin evaluating its argument immediately
//in separate global thread, or it could simply hold
//onto his argument until `get` called and begin evaluation
//then. In this example, if we want any degree of parallelism,
//we require that unit begin evaluating its argument concurrently
//and return immediately instead of wait `get` called.
//This because function argument in scala evaluated from left to(1)
//right, so if unit delays until `get` called (while evaluating
//argument for + function on last line of else function),
//we will both spawn the parallel computation and wait for it
//to finish before spawning the second parallel computation 
//(the second argument to + function).
//But if `unit` begin evaluating its argument concurrently, then
//calling `get` arguably breaks referential transparency.
//We can see it by replacing sumL and sumR with their definition
//in last line of else case. In this case , we get result
//as if we delay until `get` as in(1) that is this become sequential.
