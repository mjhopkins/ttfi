package ttfi

trait IntroHOT {
  sealed trait Var
  case object VZ extends Var
  case class VS(v: Var) extends Var

  sealed trait Exp
  case class V(v: Var) extends Exp
  case class B(b: Boolean) extends Exp
  case class L(e: Exp) extends Exp
  case class A(e: Exp, f: Exp) extends Exp

  def lookp[A](v: Var, env: List[A]): A = (v, env) match {
    case (VZ, x :: _)    => x
    case (VS(w), _ :: n) => lookp(w, n)
    case _               => throw new Exception("variable not found in environment")
  }
  //  def eval0(env: List[Boolean], exp: Exp): Any = exp match {
  //    case V(v)    => lookp(v, env)
  //    case B(b)    => b
  //    case L(e)    => (x:Boolean) => eval0(x::env, e)
  //    case A(e, f) => eval0(env, e)(eval0(env, f)) // doesn't type check
  //  }
  //  // return type is Any

  sealed trait U
  case class UB(b: Boolean) extends U
  case class UA(run: U => U) extends U

  def eval(env: List[U], exp: Exp): U = exp match {
    case V(v)      => lookp(v, env)
    case B(b)      => UB(b)
    case L(e)      => UA(u => eval(u :: env, e))
    case A(e1, e2) => eval(env, e1) match {
      case UA(f) => f(eval(env, e2))
      case _     => throw new Exception("Tried to apply something that wasn't an abstraction")
    }
  }

  val ti1 = A(L(V(VZ)), B(true))
  val ti1_eval = eval(List(), ti1)

  val ti2a = A(B(true), B(false))
  def ti2a_eval = eval(List(), ti2a)

  val ti2o = A(L(V(VS(VZ))), B(true))
  def ti2o_eval = eval(List(), ti2o)
}
object IntroHOT extends IntroHOT
object IntroHOTMain extends Main with IntroHOT {
  println(ti1)
  println(ti1_eval)
  println(ti2a)
//  println(ti2a_eval)
  println(ti2o)
//  println(ti2o_eval)
}
