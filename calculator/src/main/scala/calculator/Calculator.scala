package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    var res = Map[String,Signal[Double]]()
    for(ne <- namedExpressions){
      val name = ne._1
      val expr = ne._2
      res += ne._1->Signal(eval(expr(),namedExpressions))
    }
    res
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match  {
      case Literal(v) => v
      case Ref(n) => eval(getReferenceExpr(n,references),references)
      case Plus(a,b)=>eval(a,references)+eval(b,references)
      case Minus(a,b)=>eval(a,references)-eval(a,references)
      case Times(a,b)=>eval(a,references)*eval(a,references)
      case Divide(a,b)=>eval(a,references)/eval(a,references)
    }
    
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
