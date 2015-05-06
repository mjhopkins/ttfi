package ttfi

import ttfi.Intro2.ExpSYM

trait ExtF {

  import ttfi.{Intro2 => F}

  trait MulSYM[Repr] {
    def mul: (Repr, Repr) => Repr
  }

  object MulSYM {
    def apply[R: MulSYM] = implicitly[MulSYM[R]]
  }

  def tfm1[R](implicit M: MulSYM[R], A: F.ExpSYM[R]) = {
    import A._
    import M._
    add(lit(7), neg(mul(lit(1), lit(2))))
  }

  def tfm2[R](implicit M: MulSYM[R], A: F.ExpSYM[R]) = {
    import A._
    import M._
    mul(lit(7), F.tf1)
  }

  implicit object intMulSYM extends MulSYM[Int] {
    def mul = _ * _
  }

  implicit object stringMulSYM extends MulSYM[String] {
    def mul = "(" + _ + " * " + _ + ")"
  }

  val tfm1_eval = F.eval(tfm1[Int])
  val tfm2_eval = F.eval(tfm2[Int])
  val tfm1_view = F.view(tfm1[String])
  val tfm2_view = F.view(tfm2[String])

  def tfl1[R: MulSYM : ExpSYM] = List(F.tf1)
  def tfl2[R: MulSYM : ExpSYM] = tfm1[R] :: tfm2[R] :: tfl1[R]

  val tfl2_eval = tfl2[Int] map F.eval
  val tfl2_view = tfl2[String] map F.view
}

object ExtF extends ExtF

object ExtFTest extends ExtF with App {
  println(tfm1_eval)
  println(tfm2_eval)
  println(tfm1_view)
  println(tfm2_view)
  println(tfl2_eval)
  println(tfl2_view)
}
