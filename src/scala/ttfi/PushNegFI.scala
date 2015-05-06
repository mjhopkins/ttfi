package ttfi

import Intro2._
import Intro1.Exp
import Intro1.Add
import Intro1.Neg
import Intro1.Lit
import ttfi.{PushNegI => I}


trait PushNegFI {
  implicit val ExpExpSYM: ExpSYM[Exp] = new ExpSYM[Exp] {
    def lit = Lit
    def neg = Neg
    def add = Add
  }

  import ExpExpSYM._

  def initialize: Exp => Exp = identity


  def finalize[R: ExpSYM]: Exp => R = {
    case Lit(n)    => ExpSYM[R].lit(n)
    case Neg(e)    => ExpSYM[R].neg(finalize[R].apply(e))
    case Add(l, r) => ExpSYM[R].add(finalize[R].apply(l), finalize[R].apply(r))
  }

  def push_neg[R: ExpSYM] = finalize[R] compose I.push_neg compose initialize
  // should try version with implicit param after function param for comparison

  val tf1_view = view(tf1[String])

  def tf1_norm[R: ExpSYM] = push_neg[R].apply(tf1[Exp])

  val tf1_norm_view = view(tf1_norm[String])
  val tf1_norm_eval = eval(tf1_norm[Int])

  def tf1n_norm[R: ExpSYM] = push_neg[R].apply(neg(tf1[Exp]))

  val tf1n_norm_view = view(tf1n_norm[String])
  val tf1n_norm_eval = eval(tf1n_norm[Int])

  def tf1nn_norm[R: ExpSYM] = push_neg[R].apply(neg(tf1n_norm[Exp]))

  val tf1nn_norm_view = view(tf1nn_norm[String])
  val tf1nn_norm_eval = eval(tf1nn_norm[Int])

  case class ExpSYMDict[R](lit_dict: Int => R, neg_dict: R => R, add_dict: (R, R) => R)
  type FinTerm[R] = ExpSYMDict[R] => R

  val view_dict = ExpSYMDict[String](i => i.toString, "(-" + _ + ")", "(" + _ + " + " + _ + ")")
  val eval_dict = ExpSYMDict[Int](identity, -_, _ + _)

  // tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))
  def `tf1'`[R]: FinTerm[R] = dict =>
    dict.add_dict(
      dict.lit_dict(8),
      dict.neg_dict(dict.add_dict(
        dict.lit_dict(1),
        dict.lit_dict(2)
      ))
    )

  val `tf1'_view` = `tf1'`(view_dict)
  val `tf1'_eval` = `tf1'`(eval_dict)

}
object PushNegFI extends PushNegFI

object PushNegFIMain extends Main with PushNegFI {

  println(PushNegFI.tf1_view)
  println(tf1_norm_view)
  println(tf1_norm_eval)
  println(tf1n_norm_view)
  println(tf1n_norm_eval)
  require(tf1_norm_view == tf1nn_norm_view, "Double neg")
  println(tf1nn_norm_view)
  println(tf1nn_norm_eval)
  require(eval(tf1[Int]) == tf1_norm_eval, "Normalization")
  require(eval(tf1[Int]) == -tf1n_norm_eval, "Normalization")
  println(`tf1'_view`)
  println(`tf1'_eval`)
}
