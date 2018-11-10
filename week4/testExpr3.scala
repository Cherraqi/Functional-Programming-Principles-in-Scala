object testExpr3 {
  def eval(e: Expr3): Int = e match {
    case Number3(n) => n
    case Sum3(e1, e2) => eval(e1) + eval(e2)
  }
  def show(e: Expr3): String = e match {
    case Number3(n) => ""+n
    case Sum3(e1, e2) => show(e1) + " + " + show(e2)
  }
  def main(args: Array[String]) = {
    val expr1 = Sum3(Number3(1), Number3(2))
    val expr2 = Sum3(Number3(3), Number3(4))
    val expr3 = Sum3(expr1, expr2)
    val expr4 = Sum3(Prod3(Number3(2), Var3("x")), Var3("y"))
    val expr5 = Prod3(Sum3(Number3(2), Var3("x")), Var3("y"))
    val expr6 = Prod3(Var3("y"), Sum3(Number3(2), Var3("x")))
    val expr7 = Prod3(expr1, expr2)
    println("Externally Defined eval and show:")
    println(show(expr1)+ " = "+ eval(expr1)) 
    println(show(expr2)+ " = "+ eval(expr2)) 
    println(show(expr3)+ " = "+ eval(expr3)) 
    println("Trait defined eval and show: ")
    println(expr1.show+ " = "+ expr1.eval) 
    println(expr2.show+ " = "+ expr2.eval) 
    println(expr3.show+ " = "+ expr3.eval) 
    
    println(expr4.show)
    println(expr5.show)
    println(expr6.show)
    println(expr7.show)

  }
}
