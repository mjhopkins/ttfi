package ttfi


trait Intro1 {
  sealed trait Exp
  case class Lit(int: Int) extends Exp
  case class Neg(exp: Exp) extends Exp
  case class Add(l: Exp, r: Exp) extends Exp

  val ti1 = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

  def eval: Exp => Int = {
    case Lit(n)    => n
    case Neg(e)    => -eval(e)
    case Add(l, r) => eval(l) + eval(r)
  }
  val ti1Eval = eval(ti1)

  type Repr = Int
  def lit: Int => Repr = identity
  def neg: Repr => Repr = e => -e
  def add: (Repr, Repr) => Repr = _ + _

  val tf1 = add(lit(8), neg(add(lit(1), lit(2))))


  def view: Exp => String = {
    case Lit(n)    => n.toString
    case Neg(e)    => "(-" + view(e) + ")"
    case Add(l, r) => "(" + view(l) + " + " + view(r) + ")"
  }
  val ti1View = view(ti1)

}
object Intro1 extends Intro1
object Intro1Main extends Main with Intro1 {
  println(ti1Eval)
  println(tf1)
  println(ti1View)
}
