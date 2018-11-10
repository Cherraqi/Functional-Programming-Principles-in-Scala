import scala.util.Random


object isort{
def main(args: Array[String]){
	val rand = List.fill(10)(Random.nextInt(20))

	def isort(xs: List[Int]): List[Int] =
	  if (xs.isEmpty) Nil
	  else insert(xs.head, isort(xs.tail))

	def insert(x: Int, xs: List[Int]): List[Int] =
	  if (xs.isEmpty || x <= xs.head) x :: xs
	  else xs.head :: insert(x, xs.tail)
	println(rand)   //Random List 
	println(isort(rand)) //sorted List with insertion sort
	}
}
