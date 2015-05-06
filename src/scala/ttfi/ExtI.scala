package ttfi



trait ExtI {

  import ttfi.{Intro1 => Old}

  sealed trait Exp
  case class EOld(exp: Old.Exp) extends Exp
  case class Mul(l: Exp, r: Exp) extends Exp

  import Old.Lit
  import Old.Add
  import Old.Neg
  import Old.ti1

  val tim2: Exp = Mul(EOld(Lit(7)), EOld(ti1))

  //// the constructors of neg (and add) only accept the old type, not our enhanced type
  //  val tim1 =
  //    EOld(Add(
  //      Lit(7),
  //      Neg(
  //        Mul(
  //          EOld(Lit(1)),
  //          EOld(Lit(2)),
  //        )
  //      )
  //    )
  //  )
}
object ExtI extends ExtI
object ExtIMain extends Main with ExtI
