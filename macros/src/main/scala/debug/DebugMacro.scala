package debug

import scala.reflect.macros._

object DebugMacro {

  def debug_impl(c: blackbox.Context)(params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val trees = params.map { param =>
      param.tree match {
        // Keep constants as-is
        case Literal(Constant(const)) =>
          reify { print(param.splice) }.tree
        case _ =>
          val paramRep = show(param.tree).split("\\.").last
          val paramRepTree = Literal(Constant(paramRep))
          val paramRepExpr = c.Expr[String](paramRepTree)
          reify { print(paramRepExpr.splice + " = " + param.splice) }.tree
      }
    }

    // Inserting ", " between trees, and a println at the end.
    val separators = (1 to trees.size - 1 map { _ => reify { print(", ") }.tree }) :+ reify { println() }.tree
    val treesWithSeparators = trees.zip(separators).flatMap(p => List(p._1, p._2))

    c.Expr[Unit](Block(treesWithSeparators.toList, Literal(Constant(()))))
  }
}


