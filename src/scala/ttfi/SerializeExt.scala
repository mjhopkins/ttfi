package ttfi

import Intro2._
import ExtF._
import Serialize.Tree
import Serialize.Leaf
import Serialize.Node
import ttfi.{Serialize => S}
import ttfi.Serialize._

import scalaz.\/

trait SerializeExt {

  implicit def treeMulSYM: MulSYM[Tree] =
    new MulSYM[Tree] {
      def mul = (e1, e2) => Node("Mul", List(e1, e2))
    }

  implicit def pairMulSYM[R1: MulSYM, R2: MulSYM]: MulSYM[(R1, R2)] =
    new MulSYM[(R1, R2)] {
      def mul = { case ((e11, e12), (e21, e22)) => (MulSYM[R1].mul(e11, e21), MulSYM[R2].mul(e12, e22)) }
    }

  val tfm1_tree = S.toTree(tfm1[Tree])
  val tfm2_tree = S.toTree(tfm2[Tree])

  def fromTreeExt[R: ExpSYM : MulSYM](self: Tree => ErrMsg \/ R)(t: Tree): ErrMsg \/ R =
    t match {
      case Node("Mul", List(e1, e2)) => liftM2(self(e1), self(e2))(MulSYM[R].mul)
      case e                         => S.fromTreeExt[R](self)(e)
    }

  def fromTree[R: ExpSYM : MulSYM] = S.fix(fromTreeExt[R])

  def tf1d_int3() = S.checkConsume(S.thrice)(fromTree[Int :&: String :&: Tree].apply(tfm1_tree))
  def tfm1d_int3() = S.checkConsume(S.thrice)(fromTree[Int :&: String :&: Tree].apply(tfm2_tree))
  def tfm2d_int3() = ()
}
object SerializeExt extends SerializeExt
object SerializeExtMain extends Main with SerializeExt {
  println(tfm1_tree)
  println(tfm2_tree)
  tf1d_int3()
  tfm1d_int3()
  tfm2d_int3()

}
