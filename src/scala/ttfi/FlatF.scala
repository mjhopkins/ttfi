package ttfi

import Intro2._
import PushNegF._

trait FlatF {
  sealed trait Ctx[+E]
  case class LCA[E](e: E) extends Ctx[E]
  case object NonLCA extends Ctx[Nothing]

  implicit def ctxExpSYM[R: ExpSYM]: ExpSYM[Ctx[R] => R] = new ExpSYM[Ctx[R] => R] {
    def lit = n => {
      case NonLCA => ExpSYM[R].lit(n)
      case LCA(e) => ExpSYM[R].add(ExpSYM[R].lit(n), e)
    }
    def neg = e => {
      case NonLCA  => ExpSYM[R].neg(e(NonLCA))
      case LCA(e3) => ExpSYM[R].add(ExpSYM[R].neg(e(NonLCA)), e3)
    }
    def add = (e1, e2) => ctx => e1(LCA(e2(ctx)))
  }

  def flata[R]: (Ctx[R] => R) => R =
    e => e(NonLCA)

  def norm[R]: (PushNegF.Ctx => Ctx[R] => R) => R = flata[R] compose pushNeg[Ctx[R] => R]
  // norm1(e[R]) = norm(e[PushNegF.Ctx => Ctx[R] => R])
  def tf3[R](implicit R: ExpSYM[R]): R = {
    import R._
    add(tf1[R], neg(neg(tf1[R])))
  }

  val tf3_view = view(tf3[String])
  val tf3_eval = eval(tf3[Int])

  def tf3_norm[R: ExpSYM] = norm(tf3[PushNegF.Ctx => Ctx[R] => R])

  val tf3_norm_view = view(tf3_norm[String])
  val tf3_norm_eval = eval(tf3_norm[Int])

  def tf4[R](implicit R: ExpSYM[R]) = {
    import R._
    def t = add(add(lit(1), lit(2)), lit(3))
    add(t, neg(t))
  }

  val tf4_view = view(tf4[String])
  val tf4_eval = eval(tf4[Int])

  def tf4_norm[R: ExpSYM] = norm(tf4[PushNegF.Ctx => Ctx[R] => R])

  val tf4_norm_view = view(tf4_norm[String])
  val tf4_norm_eval = eval(tf4_norm[Int])

}
object FlatF extends FlatF
object FlatFMain extends Main with FlatF {

  println(tf3_view)
  println(tf3_eval)
  println(tf3_norm_view)
  println(tf3_norm_eval)
  require(tf3_eval == tf3_norm_eval, "Normalization")
  // normalizing a normal form does not change it
  require(view(norm(tf3_norm[PushNegF.Ctx => Ctx[String] => String])) == tf3_norm_view, "Normalization")
  println(tf4_view)
  println(tf4_eval)
  println(tf4_norm_view)
  println(tf4_norm_eval)
  require(tf4_eval == tf4_norm_eval, "Normalization")
  require(view(norm(tf4_norm[PushNegF.Ctx => Ctx[String] => String])) == tf4_norm_view, "Normalization")
}
