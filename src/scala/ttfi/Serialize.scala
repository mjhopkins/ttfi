package ttfi

import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.apply._
import Intro2._

trait Serialize {

  sealed trait Tree
  case class Leaf(s: String) extends Tree
  case class Node(s: String, children: List[Tree]) extends Tree

  implicit object treeExprSYM extends ExpSYM[Tree] {
    def lit = n => Node("Lit", List(Leaf(n.toString)))
    def neg = e => Node("Neg", List(e))
    def add = (l, r) => Node("Add", List(l, r))
  }

  def toTree: Tree => Tree = identity

  val tf1_tree = toTree(tf1[Tree])

  type ErrMsg = String

  // emulate the niceness of Haskell
  def liftM1[A, B]: (ErrMsg \/ A) => (A => B) => (ErrMsg \/ B )= Functor[({type l[a] = ErrMsg \/ a})#l].map _
  def liftM2[A, B] = Apply[({type l[a] = ErrMsg \/ a})#l].apply2[A, A, B] _

  trait SafeRead[T] {def safeRead(s: String): ErrMsg \/ T }
  implicit class SafeReadOps(s: String) {
    def read[T](implicit read: SafeRead[T]) = read.safeRead(s)
  }
  implicit object intSafeRead extends SafeRead[Int] {
    def safeRead(s: String) = try s.toInt.right
    catch { case e: NumberFormatException => ("Read error: " + e.getMessage).left }
  }

  /*
  ARGH
  annoying when implicit params come before normal params.
  inference stops working
   */

  def fromTree[B](t: Tree)(implicit B: ExpSYM[B]): ErrMsg \/ B = {
    import B._
    t match {
      case Node("Lit", List(Leaf(n))) => n.read[Int] map lit
      case Node("Neg", List(e))       =>
        val value: ErrMsg \/ B = fromTree[B](e)
        value map neg
      case Node("Add", List(l, r))    =>
        val v1: ErrMsg \/ B = fromTree[B](l)
        val v2: ErrMsg \/ B = fromTree[B](r)
        //        for {
        //          r1 <- v1
        //          r2 <- v2
        //        } yield add(r1, r2)

        //        Applicative[({type l[a] = ErrMsg \/ a})#l].apply2(v1, v2)(add)
        (v1 |@| v2)(add)
      case other                      => ("invalid tree " + other).left[B]
    }
  }

  val tfl1Deserialised_eval = fromTree[Int](tf1_tree)
    .fold(err => "Error: " + err, expr => eval(expr).toString)


  trait Wrapped {
    def unwrap[Repr]: ExpSYM[Repr] => Repr
    def unwrapi[Repr: ExpSYM]: Repr = unwrap(implicitly[ExpSYM[Repr]])
  }
  object Wrapped {
    def lit(i: Int): Wrapped = new Wrapped {
      def unwrap[Repr] = e => e.lit(i)
    }
    def neg(exp: Wrapped): Wrapped = new Wrapped {
      def unwrap[Repr] = sym => {
        val v = exp.unwrap[Repr](sym)
        sym.neg(v)
      }
    }
    def add(l: Wrapped, r: Wrapped) = new Wrapped {
      def unwrap[Repr] = sym => {
        val v1 = l.unwrap[Repr](sym)
        val v2 = r.unwrap[Repr](sym)
        sym.add(v1, v2)
      }
    }
  }
  def fromTreeW: Tree => ErrMsg \/ Wrapped = {
    case Node("Lit", List(Leaf(n))) => n.read[Int] map Wrapped.lit
    case Node("Neg", List(e))       => fromTreeW(e) map Wrapped.neg
    case Node("Add", List(l, r))    => (fromTreeW(l) |@| fromTreeW(r))(Wrapped.add)
    case other                      => s"invalid tree $other".left
  }

  val tfl1DeserializedEvewWrapped = {
    fromTreeW(tf1_tree) fold (
      err => "Error: " + err,
      expr => (
        eval(expr.unwrapi[Int]),
        view(expr.unwrapi[String])
        )
      )
  }
  // with Wrapped, we gain the ability to interpret the same deserialised term multiple times, in different context
  // but we still haven't solved the expression problem

  implicit def pairExpSym[R1: ExpSYM, R2: ExpSYM]: ExpSYM[(R1, R2)] = new ExpSYM[(R1, R2)] {
    def lit = n => (ExpSYM[R1].lit(n), ExpSYM[R2].lit(n))
    def neg = { case (x, y) => (ExpSYM[R1].neg(x), ExpSYM[R2].neg(y)) }
    def add = { case ((e11, e12), (e21, e22)) => (ExpSYM[R1].add(e11, e21), ExpSYM[R2].add(e12, e22)) }
  }

  type :&:[A, B] = (A, B)

  def duplicate[R1: ExpSYM, R2: ExpSYM]: ((R1, R2)) => (R1, R2) = identity

  def dup[R: ExpSYM, S: ExpSYM] = ???
  //TODO replace unit with IO
  def checkConsume[A, B, R](f: B => Unit): ErrMsg \/ B => Unit = {
    case -\/(e) => println("Error: " + e)
    case \/-(x) => f(x)
  }

  def dupConsume[R1: ExpSYM, R2: ExpSYM, S](ev: R1 => S, x: (R1, R2)): R2 = {
    val (x1, x2) = duplicate[R1, R2].apply(x)
    println(ev(x1))
    x2
  }

  def intString: (Int, String) = (tf1[Int], tf1[String])
  def intStringTree = (tf1[Int], tf1[String], tf1[Tree])

  // deserialized value consumed by 3 different interpreters
  val consumeThrice: ErrMsg \/ (Int, (String, Tree)) => Unit = checkConsume(thrice)
  def tf1deserializedInt3() = consumeThrice(fromTree[Int :&: String :&: Tree](tf1_tree))


  def thrice(x: Int :&: String :&: Tree) = {
    println()
    val y = dupConsume(eval, x)
    val z = dupConsume(view, y)
    println(toTree(z))
    println()
  }

  // NB implicit arg *after* Tree argument
  def fromTreeExt[R: ExpSYM](self: Tree => ErrMsg \/ R)(t: Tree): ErrMsg \/ R = t match {
    case Node("Lit", List(Leaf(n))) => n.read[Int] map ExpSYM[R].lit
    case Node("Neg", List(e))       => self(e) map ExpSYM[R].neg
    case Node("Add", List(e1, e2))  => liftM2(self(e1), self(e2))(ExpSYM[R].add)
    case e                          => s"Invalid tree: $e".left
  }

  def fix[A, B](f: (A => B) => (A => B)): A => B = f(fix(f))(_)

  def fromTree2[R: ExpSYM] = fix(fromTreeExt[R])

  def tf1E_int3() = consumeThrice(fromTree2[Int :&: String :&: Tree].apply(tf1_tree))
  def tfxE_int3() = consumeThrice(fromTree2[Int :&: String :&: Tree].apply(Node("Lit", List(Leaf("1"), Leaf("2")))))
}

object Serialize extends Serialize
object SerializeMain extends Main with Serialize {
  println(tf1_tree)
  println(tfl1Deserialised_eval)
  println(tfl1DeserializedEvewWrapped)
  tf1deserializedInt3()
  tf1E_int3()
  tfxE_int3()
}
