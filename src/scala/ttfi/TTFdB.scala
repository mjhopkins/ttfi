
package ttfi

trait TTFdB {
  trait Symantics[Repr[_, _]] {
    def int[H](n: Int): Repr[H, Int]
    def add[H](e1: Repr[H, Int], e2: Repr[H, Int]): Repr[H, Int]
    def z[H, A]: Repr[(A, H), A]
    def s[H, A, B](v: Repr[H, A]): Repr[(B, H), A]
    def lam[H, A, B](e: Repr[(A, H), B]): Repr[H, A => B]
    def app[H, A, B](e1: Repr[H, A => B], e2: Repr[H, A]): Repr[H, B]
  }

  def td1[R[_, _], H](implicit R: Symantics[R]): R[H, Int] = {
    import R._
    add(int[H](1), int[H](2))
  }

  def td2[R[_, _], H](implicit R: Symantics[R]): R[H, Int => Int] = {
    import R._
    lam(add(z[H, Int], z[H, Int]))
  }

  def td2o[R[_, _], H](implicit R: Symantics[R]): R[(Int, H), Int => Int] = {
    import R._
    val e2: R[(Int, (Int, H)), Int] = s(z[H, Int])
    val e: R[(Int, (Int, H)), Int] = add(z[(Int, H), Int], e2)
    lam[(Int, H), Int, Int](e)
  }

  //  def td2a[R[_, _], H](implicit R: Symantics[R]) = {
  //    import R._
  //    // polymorphic expression cannot be instantiated to expected type
  //    app(int(1), int(2))
  //  }

  def td3[R[_, _], H](implicit R: Symantics[R]): R[H, (Int => Int) => Int] = {
    import R._
    lam(add(app(z[H, Int => Int], int[(Int => Int, H)](1)), int(2)))
  }

  case class R[H, A](run: H => A)
  // try as a newtype

  implicit val symanticsR: Symantics[R] = new Symantics[R] {
    def int[H](n: Int) = R(_ => n)
    def add[H](e1: R[H, Int], e2: R[H, Int]) = R(h => e1.run(h) + e2.run(h))

    def z[H, A] = R { case (a, _) => a }
    def s[H, A, B](v: R[H, A]) = R { case (_, h) => v.run(h) }
    def lam[H, A, B](e: R[(A, H), B]) = R(h => a => e.run((a, h)))
    def app[H, A, B](e1: R[H, (A) => B], e2: R[H, A]) = R(h => e1.run(h)(e2.run(h)))
  }

  def eval[A](e: R[Unit, A]) = e.run(())

  val td1_eval = eval(td1[R, Unit])
  val td2_eval = eval(td2[R, Unit])

  val `td2_eval(21)` = td2_eval(21)

  //  val td2o_eval = eval(td2o)

  val td3_eval = eval(td3[R, Unit])
  val `td3_eval(_ * 40)` = td3_eval((_: Int) * 40)

  case class S[H, A](run: Int => String)

  implicit def symanticsS = new Symantics[S] {
    def int[H](n: Int) = S(_ => n.toString)
    def add[H](e1: S[H, Int], e2: S[H, Int]) = S(h => "(" + e1.run(h) + " + " + e2.run(h) + ")")
    def z[H, A] = S(h => "x" + (h - 1))
    def s[H, A, B](v: S[H, A]) = S(h => v.run(h - 1))
    def lam[H, A, B](e: S[(A, H), B]) = S { h =>
      val x = "x" + h
      "(" + x + " => " + e.run(h + 1) + ")"
    }
    def app[H, A, B](e1: S[H, A => B], e2: S[H, A]) = S(h => e1.run(h) + "(" + e2.run(h) + ")")
  }

  def view[A](e: S[Unit, A]): String = e.run(0)

  val td1_view = view(td1[S, Unit])

  val td2_view = view(td2[S, Unit])

  val td3_view = view(td3[S, Unit])


  trait MulSYM[Repr[_, _]] {
    def mul[H](e1: Repr[H, Int], e2: Repr[H, Int]): Repr[H, Int]
  }

  trait BoolSYM[Repr[_, _]] {
    def bool[H](b: Boolean): Repr[H, Boolean]
    def if_[H, A](cond: Repr[H, Boolean], ifTrue: Repr[H, A], ifFalse: Repr[H, A]): Repr[H, A]
    def leq[H](e1: Repr[H, Int], e2: Repr[H, Int]): Repr[H, Boolean]
  }

  trait FixSYM[Repr[_, _]] {
    def fix[H, A](f: Repr[(A, H), A]): Repr[H, A]
  }

//  def tpow[Repr](implicit S: Symantics[R], B: BoolSYM[R], M: MulSYM[R], F: FixSYM[R]) = {
//    import S._, B._, M._, F._
//// lam()
//
//  }

  implicit val mulSymR = new MulSYM[R] {
    def mul[H](e1: R[H, Int], e2: R[H, Int]) = R(h => e1.run(h) * e2.run(h))
  }

  implicit val mulSymS = new MulSYM[S] {
    def mul[H](e1: S[H, Int], e2: S[H, Int]) = S(h => "(" + e1.run(h) + " * " + e2.run(h) + ")")
  }
  
  implicit val boolSymR = new BoolSYM[R] {
    def bool[H](b: Boolean) = R (_ => b)
    def if_[H, A](cond: R[H, Boolean], ifTrue: R[H, A], ifFalse: R[H, A]) = R(h => if (cond.run(h)) ifTrue.run(h) else ifFalse.run(h))
    def leq[H](e1: R[H, Int], e2: R[H, Int]) = R(h => e1.run(h) <= e2.run(h))
  }

  implicit val boolSymS = new BoolSYM[S] {
    def bool[H](b: Boolean) = S(_ => b.toString)
    def if_[H, A](cond: S[H, Boolean], ifTrue: S[H, A], ifFalse: S[H, A]) = S(h => "if (" + cond.run(h) + ") then " + ifTrue.run(h) + " else " + ifFalse.run(h))
    def leq[H](e1: S[H, Int], e2: S[H, Int]) = S(h => e1.run(h) + " <= " + e2.run(h))
  }


  def tdm1[R[_, _], H](implicit R: Symantics[R], M: MulSYM[R]): R[H, Int] = {
    import R._
    import M._
    mul(int[H](6), add(int[H](3), int[H](4)))
  }

  val tdm1_eval = eval(tdm1[R, Unit])
  val tdm1_view = view(tdm1[S, Unit])

  def tdb1[R[_, _], H](implicit R: Symantics[R], B: BoolSYM[R], M:MulSYM[R]): R[H, Int => Int] = {
    import R._
    import B._
    import M._
    lam(if_(leq(z[H,Int], int(10)), mul(z[H, Int],z[H, Int]), int(63)))
  }

  val tdb1_eval = eval(tdb1[R, Unit])
  val `tdb1_eval(9)` = tdb1_eval(9)
  val `tdb1_eval(10)` = tdb1_eval(10)
  val `tdb1_eval(11)` = tdb1_eval(11)
  val tdb1_view = view(tdb1[S, Unit])
}

object TTFdB extends TTFdB

object TTFdBMain extends Main with TTFdB {
  println(td1_eval)
  println(td1_view)
  println(`td2_eval(21)`)
  println(td2_view)
  println(`td3_eval(_ * 40)`)
  println(td3_view)

  println(tdm1_eval)
  println(tdm1_view)

  println(`tdb1_eval(9)`)
  println(`tdb1_eval(10)`)
  println(`tdb1_eval(11)`)
  println(tdb1_view)

}
