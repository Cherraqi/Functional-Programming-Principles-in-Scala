/** Higher Order List Functions
 */
object Lists extends App {
	val nums = List(2, -4, 5, 7, 1)
	val fruits = List("apple", "pineapple", "orange", "banana")
		  println(nums)
		  println(fruits)

	def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
		case Nil => xs
		case y :: ys => y * factor :: scaleList(ys, factor)
	}
	
	def squareListViaMap(xs: List[Int]): List[Int] = xs map (x => x*x)

	def posElem(xs: List[Int]): List[Int] = xs match {
		case Nil => xs
		case y :: ys => if(y > 0) y :: posElem(ys) else posElem(ys)
	}

	def posElemViaFilter(xs: List[Int]): List[Int] = xs filter (_ >0)

	println(posElem(List(-2,-1,0,1,2)))
	posElemViaFilter(List(-2,-1,0,1,2))
	nums filter (_ > 0) 
	nums filterNot (_ > 0) 
	nums partition ( _ > 0) 
	nums takeWhile (_ > 0)                    
	List(1,2,3,-4) takeWhile (_ > 0)         
	
	nums dropWhile (_ > 0)                    
	List(1,2,3,-4) dropWhile (_ > 0)          
	nums span (_ > 0)

	//A function that packs consecutive duplicates of list elements into sublists
	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 => {
			val part = xs span(_ == x)
			(part._1) :: pack(part._2)
		}
	} 
	def data = List("a", "a", "a", "b", "c", "c", "a")
	pack(data) 


	def encode[T](xs: List[T]): List[(T, Int)] = {
	  	val part = pack(xs)
	  	part map (a => (a.head, a.length))
	 	 } 
                                      
	encode(data) 


//proof of the following distribution law for map over concatenation
	val xs = List(1,2)                       
	val ys = List(2,3)                       
	val zs = List(3,4)                       
	def f = (a: Int) => a*a                  
	
	/* Prove Associative law of concat */
	(xs ++ ys) ++ zs                         
	xs ++ (ys ++ zs)                       
	
	/* Prove Distribution law of map */
	//base case
	Nil map f                              
	
	//induction step
	(xs ++ ys) map f                         
        (xs map f) ++ (ys map f) 

      
	
 
	
}
