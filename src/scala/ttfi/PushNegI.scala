package ttfi

import Intro1._

trait PushNegI {

  def push_neg: Exp => Exp = {
    case e@Lit(_)       => e
    case e@Neg(Lit(_))  => e
    case Neg(Neg(e))    => push_neg(e)
    case Neg(Add(l, r)) => Add(push_neg(Neg(l)), push_neg(Neg(r)))
    case Add(l, r)      => Add(push_neg(l), push_neg(r))
  }

  //  val ti1View = view(ti1)
  val ti1_norm = push_neg(ti1)
  val ti1_norm_eval = eval(ti1_norm)
  val ti1_norm_view = view(ti1_norm)

  val ti1n_norm = push_neg(Neg(ti1))
  val ti1n_norm_view = view(ti1n_norm)
  val ti1n_norm_eval = eval(ti1n_norm)

  val ti1nn_norm = push_neg(Neg(ti1n_norm))
  val ti1nn_norm_view = view(ti1nn_norm)
  val ti1nn_norm_eval = eval(ti1nn_norm)
}

object PushNegI extends PushNegI

object PushNegIMain extends Main with PushNegI {
  println(ti1View)
  println(ti1_norm_view)
  println(ti1_norm_eval)

  println(ti1n_norm_view)
  println(ti1n_norm_eval)

  println(ti1nn_norm_view)
  println(ti1nn_norm_eval)
  require(ti1_norm_view == ti1nn_norm_view, "Double negation")
  require(eval(ti1) == ti1_norm_eval, "Normalisation")
  require(eval(ti1) == -ti1n_norm_eval, "Negation")
}
