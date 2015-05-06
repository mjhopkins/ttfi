package ttfi

trait Intro2 {
  trait ExpSYM[Repr] {
    def lit: Int => Repr
    def neg: Repr => Repr
    def add: (Repr, Repr) => Repr
  }

  object ExpSYM {
    @inline def apply[R](implicit ev: ExpSYM[R]) = ev
  }

  def tf1[R](implicit R: ExpSYM[R]): R = {
    import R._
    add(lit(8), neg(add(lit(1), lit(2))))
  }

  implicit object intExpSYm extends ExpSYM[Int] {
    def lit = identity
    def neg = -_
    def add = _ + _
  }

  val eval: Int => Int = identity

  val tf1_eval = eval(tf1[Int])

  implicit object stringExpSYM extends ExpSYM[String] {
    def lit = _.toString
    def neg = "(-" + _ + ")"
    def add = "(" + _ + " + " + _ + ")"
  }

  def view: String => String = identity

  val tf1_view = view(tf1[String]) // oh, scala is dumb

}
object Intro2 extends Intro2
object Intro2Main extends Main with Intro2 {
  println(tf1_eval)
  println(tf1_view)
}