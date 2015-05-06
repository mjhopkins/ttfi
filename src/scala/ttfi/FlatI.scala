
package ttfi

import ttfi.Intro1._
import ttfi.PushNegI._

trait FlatI {
  /*
   Flatten the additions using the associativity
   * (A + B) + R => A + (B + R)
   Draw the trees for the former and the latter
   The goal is to convert the addition tree to the right-skewed form
     The transformation is assumed to be performed after the negation is
     pushed down

   Previously, expressions were constructed according to this grammar:
     * General grammar of expressions
     *     e ::= int | neg e | add e e
     *
     * Restricted grammar now:
     *     e ::= factor | add factor e
     *     factor ::= int | neg int
     Now, only integer literals can be negated, and only once.

     It is an expression transformer
  */

  def flata: Exp => Exp = {
    case e: Lit               => e
    case e: Neg               => e
    case Add(Add(e1, e2), e3) => flata(Add(e1, Add(e2, e3)))
    case Add(e1, e2)          => Add(e1, flata(e2))
  }

  /*
   Why is this terminating?
   The last two clauses express the lexicographic ordering
   on left-depth, total depth.
   Is this code correct?
   */
  def norm: Exp => Exp = flata compose push_neg

  val ti3 = Add(ti1, Neg(Neg(ti1)))

  val ti3_view = view(ti3)
  val ti3_eval = eval(ti3)

  val ti3_norm = norm(ti3)

  val ti3_norm_view = view(ti3_norm)
  val ti3_norm_eval = eval(ti3_norm)

}
object FlatI extends FlatI
object FlatIMain extends Main with FlatI {

  println(ti3_view)
  println(ti3_eval)
  println(ti3_norm_view)
  println(ti3_norm_eval)
  require(ti3_eval == ti3_norm_eval, "Normalization")
}
