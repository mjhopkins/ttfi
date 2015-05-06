
package ttfi

import Intro2._
import PushNegF._
import ExtF._

trait PushNegFExt {
  implicit def ctxMulSYM[R: MulSYM]: MulSYM[Ctx => R] = new MulSYM[Ctx => R] {
    def mul = (a, b) => {
      case Pos => MulSYM[R].mul(a(Pos), b(Pos))
      case Neg => MulSYM[R].mul(a(Pos), b(Neg))
    }
  }

  // recall what extended terms look like
  val tfm1_view = view(tfm1[String])
  val tfm1_eval = eval(tfm1[Int])

  def tfm1_norm[R: ExpSYM : MulSYM] = pushNeg(tfm1[Ctx => R])
  val tfm1_norm_view = view(tfm1_norm[String])
  val tfm1_norm_eval = eval(tfm1_norm[Int])

  def tfm1n_norm[R: ExpSYM : MulSYM] = pushNeg(ExpSYM[Ctx => R].neg(tfm1[Ctx => R]))
  val tfm1n_norm_view = view(tfm1n_norm[String])
  val tfm1n_norm_eval = eval(tfm1n_norm[Int])

  // negate the already-negated term
  def tfm1nn_norm[R: ExpSYM : MulSYM] = pushNeg(ExpSYM[Ctx => R].neg(tfm1n_norm[Ctx => R]))
  val tfm1nn_norm_view = view(tfm1nn_norm[String])
  val tfm1nn_norm_eval = eval(tfm1nn_norm[Int])

  // the same for tmf2
  val tfm2_view = view(tfm2[String])
  val tfm2_eval = eval(tfm2[Int])

  def tfm2_norm[R: ExpSYM : MulSYM] = pushNeg(tfm2[Ctx => R])
  val tfm2_norm_view = view(tfm2_norm[String])
  val tfm2_norm_eval = eval(tfm2_norm[Int])

  def tfm2n_norm[R: ExpSYM : MulSYM] = pushNeg(ExpSYM[Ctx => R].neg(tfm2[Ctx => R]))
  val tfm2n_norm_view = view(tfm2n_norm[String])
  val tfm2n_norm_eval = eval(tfm2n_norm[Int])

  // negate the already-negated term
  def tfm2nn_norm[R: ExpSYM : MulSYM] = pushNeg(ExpSYM[Ctx => R].neg(tfm2n_norm[Ctx => R]))
  val tfm2nn_norm_view = view(tfm2nn_norm[String])
  val tfm2nn_norm_eval = eval(tfm2nn_norm[Int])
}

object PushNegFExt extends PushNegFExt

object PushNegFExtMain extends Main with PushNegFExt {

  println(PushNegF.tf1_norm_view) // old terms still work

  println(PushNegFExt.tfm1_view)
  println(PushNegFExt.tfm1_eval)
  println(tfm1_norm_view)
  println(tfm1_norm_eval)
  println(tfm1n_norm_view)
  println(tfm1n_norm_eval)
  println(tfm1nn_norm_view)
  println(tfm1nn_norm_eval)
  require(tfm1_norm_view == tfm1nn_norm_view, "Double neg")
  require(PushNegFExt.tfm1_eval == tfm1_norm_eval, "Normalization")
  require(PushNegFExt.tfm1_eval == -tfm1n_norm_eval, "Normalization")

  println(PushNegFExt.tfm2_view)
  println(PushNegFExt.tfm2_eval)
  println(tfm2_norm_view)
  println(tfm2_norm_eval)
  println(tfm2n_norm_view)
  println(tfm2n_norm_eval)
  println(tfm2nn_norm_view)
  println(tfm2nn_norm_eval)
  require(tfm2_norm_view == tfm2nn_norm_view, "Double neg")
  require(PushNegFExt.tfm2_eval == tfm2_norm_eval, "Normalization")
  require(PushNegFExt.tfm2_eval == -tfm2n_norm_eval, "Normalization")

}
