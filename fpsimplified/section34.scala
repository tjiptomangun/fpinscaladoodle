//How to write a class that can be used in a for expression
import scala.collection.mutable.ArrayBuffer;
case class Sequence[A] (initialElems: A*) {
	private val elems = scala.collection.mutable.ArrayBuffer[A] ();
	elems ++= initialElems;

	def map [B](f: A => B): Sequence[B] = {
		val abMap: scala.collection.mutable.ArrayBuffer[B] = elems.map(f);
		new Sequence(abMap.toList: _*);
	}

	def foreach(block: A => Unit): Unit = {
		elems.foreach(block);
	}

	def withFilter(p: A => Boolean): Sequence[A] = {
		val tmpArrayBuffer = elems.filter(p);
		new Sequence(tmpArrayBuffer.toList: _*);	
	}
	
	def flatMap [B] (f: A => Sequence[B]): Sequence[B] = {
		val mapRes: Sequence[Sequence[B]] = map(f)
		flattenLike(mapRes);
	}

	def flattenLike[B](seqOfSeq: Sequence[Sequence[B]]): Sequence[B] = {
		var xs = ArrayBuffer[B]()
		for (listB: Sequence[B] <- seqOfSeq) {
			for(e <- listB) {
				xs += e;
			}
		}
		Sequence(xs.toList: _*)
	}
}

val ints = Sequence(1, 2, 3)

for {
	i <- ints
	if (i < 2)
}yield( i * 3);


case class Person(name: String)

val myFriends = Sequence(
	Person("Adam"),
	Person("David"),
	Person("Frank")
)

val adamsFriends = Sequence(
	Person("Nick"),
	Person("David"),
	Person("Frank")
)

val mutualFriends = for {
	myFriend <- myFriends
	adamsFriend <- adamsFriends
	if (myFriend.name == adamsFriend.name)
} yield myFriend

