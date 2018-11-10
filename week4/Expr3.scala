
import week4._

/*
 * Pattern Matching Decomposition Implementation - 
 * pattern matching automates the deconstruction, which is precisely what
 * decomposition needs to do.
 */
trait Expr3 {
  def eval: Int = this match {
    case Number3(n) => n
    case Sum3(e1, e2) => e1.eval + e2.eval
    case Prod3(e1, e2) => e1.eval * e2.eval 
  }
  def show: String = this match {
    case Number3(n) => ""+n
    case Sum3(e1, e2) => e1.show + " + " + e2.show
    case Prod3(Sum3(e1, e2), Sum3(e3, e4)) => "("+e1.show + " + " + e2.show + ")"+" * " + "("+e3.show + " + " + e4.show + ")"
    case Prod3(Sum3(e1, e2), e3) => "("+e1.show + " + " + e2.show + ")"+" * "+ e3.show
    case Prod3(e1, Sum3(e2, e3)) => e1.show + " * " + "("+e2.show + " + " + e3.show + ")"
    case Prod3(e1, e2) => e1.show + " * " + e2.show
    case Var3(s) => s
  }

}
case class Number3(n: Int) extends Expr3
case class Sum3(e1: Expr3, e2: Expr3) extends Expr3
case class Var3(x: String) extends Expr3
case class Prod3(e1: Expr3, e2: Expr3) extends Expr3

/* 
 * Scala defines the following companion Objects automatically
object Number3 {
  def apply(n: Int) = new Number3(n)
}
object Sum3 {
  def apply(e1: Expr3, e2: Expr3) = new Sum3(e1, e2)
}

 * So you can write Number3(1) instead of new Number3(1)
 */


  

