package ttfi

import scala.language.{higherKinds, implicitConversions}
import Intro2._

trait PushNegF {
  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx

  implicit def ctxExpSYM[R](implicit R: ExpSYM[R]): ExpSYM[Ctx => R] =
    new ExpSYM[Ctx => R] {
      def lit = n => {
        case Pos => R.lit(n)
        case Neg => R.neg(R.lit(n))
      }
      def neg = e => {
        case Pos => e(Neg)
        case Neg => e(Pos)
      }
      def add = { case (a, b) => ctx => R.add(a(ctx), b(ctx)) }
    }

  //def tf1[R](implicit R: ExpSYM[R]): R
  case class ES[R](exp: ExpSYM[R] => R)

  //  how to write push_neg?
  def pushNeg[R](e: Ctx => R): R = e(Pos)
  //  def pushNeg[R: ExpSYM](e: alg[ExpSYM]) = e.apply[Ctx => R].apply(Pos)

  def tf1_norm[R: ExpSYM]: R = tf1[Ctx => R].apply(Pos)
  val tf1_norm_view = view(tf1_norm[String])
  val tf1_norm_eval = eval(tf1_norm[Int])

  def tf1n_norm[R: ExpSYM]: R = pushNeg(ExpSYM[Ctx => R].neg(tf1[Ctx => R]))
  val tf1n_norm_view = view(tf1n_norm[String])
  val tf1n_norm_eval = eval(tf1n_norm[Int])

  // negate the already-negated term
  def tf1nn_norm[R: ExpSYM]: R = pushNeg(ExpSYM[Ctx => R].neg(tf1n_norm[Ctx => R]))
  val tf1nn_norm_view = view(tf1nn_norm[String])
  val tf1nn_norm_eval = eval(tf1nn_norm[Int])
}
object PushNegF extends PushNegF

object PushNegFMain extends Main with PushNegF {
  println(tf1_view)
  println(tf1_norm_view)
  println(tf1_norm_eval)
  println(tf1n_norm_view)
  println(tf1n_norm_eval)

  require(tf1_norm_view == tf1nn_norm_view, "Double neg")

  println(tf1nn_norm_view)
  println(tf1nn_norm_eval)

  require(eval(tf1[Int]) == tf1_norm_eval, "Normalization")
  require(eval(tf1[Int]) == -tf1n_norm_eval, "Normalization")
}
