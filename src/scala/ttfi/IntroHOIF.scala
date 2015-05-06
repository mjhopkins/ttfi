package ttfi

import shapeless.Poly1

trait IntroHOIF {
  sealed trait Var[-Env, +T]
  case class VZ[Env, T]() extends Var[(T, Env), T]
  case class VS[Env, T, A](prev: Var[Env, T]) extends Var[(A, Env), T]

  def mkVZ[Env, T]: Var[(T, Env), T] = VZ[Env, T]()
  def mkVS[Env, T, A](prev: Var[Env, T]): Var[(A, Env), T] = VS[Env, T, A](prev)

  sealed trait Exp[-Env, +T]
  case class B[Env](b: Boolean)                                  extends Exp[Env, Boolean]
  case class V[Env, T](v: Var[Env, T])                           extends Exp[Env, T]
  case class L[Env, A, B](e: Exp[(A, Env), B])                   extends Exp[Env, A => B] {type X = A}
  case class A[Env, A, B](e1: Exp[Env, A => B], e2: Exp[Env, A]) extends Exp[Env, B]

  def mkB[Env](bb: Boolean): Exp[Env, Boolean] = B(bb)
  def mkV[Env, T](v: Var[Env, T]): Exp[Env, T] = V(v)
  def mkL[Env, A, B](e: Exp[(A, Env), B]): Exp[Env, A => B] = L(e)
  def mkA[Env, A, B](e1: Exp[Env, A => B], e2: Exp[Env, A]): Exp[Env, B] = A(e1, e2)

  def lookp[Env, T](v: Var[Env, T], env: Env): T = (v, env) match {
    case (VZ(), (x, _))  => x
    case (VS(w), (_, n)) => lookp(w, n)
    case _               => throw new Exception("variable not found in environment")
  }

  def eval[Env, T](env: Env, exp: Exp[Env, T]): T =
    exp match {
      case V(v)      => lookp(v, env)
      case B(b)      => b
      case ll@L(e)   => (x: ll.X) => eval((x, env), e)
      case A(e1, e2) => eval(env, e1)(eval(env, e2))
    }

  def ti1[Env]: Exp[Env, Boolean] =
    A(mkL(mkV(mkVZ[Env, Boolean])),B(true))


  def vz[A, B](a: A, b: B): A = a
  def vs[A, B, C](vp: A => B): (C, A) => B = { case (_, envr) => vp(envr) }
  def b[A](bv: Boolean): A => Boolean = env => bv
  def l[A, B, C](e: (A, B) => C): B => A => C = env => x => e(x, env)
  def a[A, B, C](e1: A => B => C, e2: A => B)(env: A) = e1(env)(e2(env))

  def tf1[A]: A => Boolean = a[A, Boolean, Boolean](l(vz), b(true))

  val tf1_eval = tf1(())

//  def tf2[A]: A => Boolean =
//    a(
//      a(
//        l(
//          l(vs(vz))
//        ),
//        b(true)
//      ),
//      b(false)
//    )
}
object IntroHOIF extends IntroHOIF
object IntroHOIFMain extends Main with IntroHOIF {
//  println(ti1_eval)
//  println(ti2_eval)
  println(tf1_eval)
//  println(tf2_eval)
//  println(tf5_eval)
}
