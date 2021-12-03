package org.learningconcurrency
object Excersice extends App{
	def compose[A, B, C] (g: B => C, f: A => B): A => C = {
		x:A => g(f(x))
	}
	def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = {
		for {
			aVal <- a
			bVal <- b
		}yield (aVal, bVal)
    }
	def check[T](xs: Seq[T])(pred: T=> Boolean): Boolean = {
		xs.foldLeft(true)((b, a) => 
			try {
				val v = pred(a)
				b && v
			}
			catch {
				case e:Exception  =>
				false
			}
		)
	}
	case class Pair[P, Q](val first:P, val second:Q)

	def f(x: Int): Double = {
		Math.sqrt(x)
	}
	def g(y: Double): String= {
		(y*y*2).toString
	}
	val res = compose(g, f)(10)
	println(s"compose $res")

	val res2 = fuse(Some(10), None)
	println(s"fuse $res2")

	val res3 = check(0 until 10)(40 / _ > 0)
	println(s"check3 $res3")

	val res4 = check(1 until 10)(40 / _ > 0)
	println(s"check4 $res4")

	val c = Pair("one", 4.5)
	c match {
		case Pair("one", 4.5) => {
			println(c)
		}
		case _ => {
			println("I don't know who you are")
		}
	}

	/**
		*
		* @param s
		* @return
		*/
	def permutate(s: String): Seq[String] = {
		/**
			* 1) y = get all string as char
      * 2) for each char, put the char as the first element
      * 3) get the rest of char (exclude selected)
      * 4) got to 2
			*/
		s.toList.zipWithIndex.map((x) => {
			val t0 = s splitAt x._2
			val c0 = t0._2.charAt(0).toString
			c0
		})
	}

	val testPerm = "bacd"
	val resPerm = permutate(testPerm)
	println(resPerm)
  /*
	val str = "abcd"
  val arrStr = str.toList
  val arrZip = arrStr.zipWithIndex
  val arrZipSplit = arrZip map ((x) => arrStr splitAt x._2)
  val arrZipSplit2 = arrZip map ((x) => {
    val x1 = x._1
    val x0 = arrStr splitAt x._2
    x0._1 ++ x0._2.tail
  })

  val arrZipSplit3 = arrZip map ((x) => {
    val x1 = x._1
    val x0 = arrStr splitAt x._2
    x1 +: (x0._1 ++ x0._2.tail)
  })

  def terong(arrStr: List[Char]): List[Char] = {
    arrStr match {
      case head::tail => {
        val arrZip = arrStr.zipWithIndex
        val arrZipSplit = arrZip map ((x) => arrStr splitAt x._2)
        arrZip map ((x) => {
          val x1 = x._1
          val x0 = arrStr splitAt x._2
          x1 +: terong(x0._1 ++ x0._2.tail)
        })
      }
      case _ => {
        List.empty: List[Char]
      }
    }
  }
*/
  //val arrZipSplitMerge = arrZipSplit map((y) => y._1 ++ y._2.tail)

  //arrStr zipWithIndex map ((x) => arrStr  splitAt x._2
	//str.toList.zipWithIndex map ((x) => {(str splitAt x._2)._2  })
  /**
    * abcd
    *
    * abcd
    * abdc
    * acdb
    * acbd
    * adbc
    * adcb
    *
    * bcda
    * bcad
    * bdac
    * bdca
    * badc
    * bacd
    *
    * cdab
    * cdba
    * cadb
    * cabd
    * cbad
    * cbda
    *
    * dabc
    * dacb
    * dbca
    * dbac
    * dcab
    * dcba
    */
  /**
    * 1) get list of char
    * 2) should return list of list of char
    * 3) the inner list contains valid permutation
    * 4) receive list of char as parameter
    * 5) if not empty
    * 5.1) put the char as the first element
    * 5.3) get the rest of char (exclude selected)
    * 5.3) got to 2
    * 6) if empty
    */
//  def terong[T](arrStr: List[T]): List[List[T]] = {
//    arrStr match {
//      case _::_=> {
//        val arrZip = arrStr.zipWithIndex
//        arrZip map ((x) => {
//          val x1 = x._1
//          val x0 = arrStr splitAt x._2
//          x0._1 ++ x0._2.tail
//
//        })
//      }
//      case _ => {
//        List(List.empty): List[List[T]]
//      }
//    }
//  }
	def terong [T](accum: (List[List[T]],  List[T]), list:List[T]): List[List[T]]  = {
		list match {
			case h :: t => {
				list.zipWithIndex.flatMap ((x) => {
					println(s"x is $x")
					val (l1, l2) = list splitAt (x._2)
					println(s"l1 is $l1")
					println(s"l2 is $l2")
					val toPush = l2.head
					println(s"toPush is $toPush ")
					val nl = l1 ++ l2.tail
					println(s"nl is $nl")
					terong((accum._1, accum._2 :+ toPush), nl)
				})
			}
			case _ =>
				accum._2 match {
					case h::t => {
						accum._1 find (_ == accum._2) match {
							case Some(x) => {
								accum._1
							}
							case _ => {
								accum._1 :+ accum._2
							}
						}
					}
					case _ => {
						accum._1
					}
				}
		}
	}
}

