object listProcessing extends App {
  //More functions on list processing
  
  val xs = List(1,2,3,4,5)                        
  //The number of elements of xs
  xs.length                                     
  xs.last //takes the last elem                   
  xs.init //takes everything but the last elem    
  List(1).init                                  
  
  	xs take 2 //take 2 prefixes of the list         
  
  	xs drop 2 //take anything but the first two prefixes
                                                 
  	xs(2) //xs apply n                              

	/** Creating new lists */
	xs ++ List(6,7)                         
	xs.reverse                              
	
	//like updating the array, but this creates a new list
	xs updated (2,-3)                        

	/** Finding elements */
	xs indexOf 3                           
	xs indexOf -3 //gives -1 if xs does not contain -3
                                                 
	xs contains 3                           
	xs contains -3                          
	
	/** Implement last
	 * last returns a new list consisting of just the last element of an input List
	*/
	def last[T](xs: List[T]): T = xs match {
		case List() => throw new Error("last of empty list")
		case List(x) => x
		case y :: ys => last(ys)
	}                                       
	
	last(List(1,2,3))                       
	last(List(1))                         
	/** Implement init
	 * init returns a new list consisting of everything in the input list except
	 * the last element
	 */
	def init[T](xs: List[T]): List[T] = xs match {
		case Nil => throw new Error("init of empty list")
		case List(x) => List()
		case y :: ys => y :: init(ys)
	}                                        
	init(List(1,2,3))                       
	init(List(1))                          
	
	/** Implement Concat
	 * contatenate two lists
	 * complexity is O(n_xs)
	*/
	def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
		case Nil => ys
		case h :: t => h :: concat(t, ys)
	}                                      
	concat(List(1,2), List(3,4))              
 	
 	/** Implement Reverse
	 * complexity is O(n*n) - There is a better way
	 */
	def reverse[T](xs: List[T]): List[T] = xs match {
		case Nil => List()
		case h :: t => reverse(t) ++ List(h)
	}                                      
	reverse(List(1,2,3))                    
	
	/** RemoveAt
	 * remove the nth element of a list xs. If n is out of bounds, return xs itself
	 */
	def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
		case Nil => List()
		case h :: Nil => {
			if(n == 0) List()
			else xs
		}
		case h :: t => {
			if(n == 0) t
			else h :: removeAt(n-1, t)
		}
	}                                        

	def removeAt2[T](n: Int, xs: List[T]): List[T] = {
		(xs take n) ::: (xs drop n+1)
	}                                        
	
	removeAt(-1, List('a', 'b', 'c', 'd'))    
	removeAt(0, List('a', 'b', 'c', 'd'))     
	removeAt(1, List('a', 'b', 'c', 'd'))     
	removeAt(3, List('a', 'b', 'c', 'd'))    
	removeAt(4, List('a', 'b', 'c', 'd'))     
	
	
	removeAt2(-1, List('a', 'b', 'c', 'd'))   
	removeAt2(0, List('a', 'b', 'c', 'd'))  
	removeAt2(1, List('a', 'b', 'c', 'd'))   
	removeAt2(3, List('a', 'b', 'c', 'd'))   
	removeAt2(4, List('a', 'b', 'c', 'd')) 
	
	/** flatten
	 * 	flatten a list structure.
	 */
	def flatten(xs: List[Any]): List[Any] = xs match {
		case Nil => Nil
		case (h: List[Any]) :: t => flatten(h) ++ flatten(t)
		case (h: Any) :: t => h :: flatten(t)
	}                                       

	flatten(List(List(1,1), 2, List(3, List(5, 8))))
                                                
	
	
}
