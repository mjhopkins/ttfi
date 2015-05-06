package ttfi


trait Intro3  {

  import ttfi.{Intro1 => I}
  import ttfi.{Intro2 => F}
  import ttfi.Intro1.Lit
  import ttfi.Intro1.Add
  import ttfi.Intro2.ExpSYM

  val til1 = List(Lit(1), Add(Lit(1), Lit(3)), I.ti1)

  val til1Eval = til1 map I.eval

  val til1View = til1 map I.view

  def tfl1[R](implicit R: ExpSYM[R]) = {
    import R._
    List(lit(1), add(lit(1), lit(3)), F.tf1)
  }

  val tfl1Eval: List[Int] = tfl1[Int] map F.eval
  val tfl1View: List[String] = tfl1[String] map F.view


}

object Intro3 extends Intro3

object Intro3Main extends Main with Intro3 {
  println(til1Eval)
  println(til1View)
  println(tfl1Eval)
  println(til1View)
}

/*
{-# LANGUAGE NoMonomorphismRestriction #-}

module Intro3 where

-- * Tagless Typed Interpreters, introduction
-- * Initial and Final, first-class

import qualified Intro1 as I
import qualified Intro2 as F
import Intro1 (Exp(..))
import Intro2 (ExpSYM(..))

-- Remind what is initial Exp, and what is a final ExpSYM.
-- Discuss `initial' vs `final' from the categorical view.
-- A bit informally, I would be calling ExpSYM `final'
-- (waving hands over the existence of the final co-algebra)


-- We can put expressions represented as a data type in the same
-- list

til1 = [Lit 1, Add (Lit 1) (Lit 3), I.ti1]

-- And we can evaluate them uniformly

til1_eval = map I.eval til1
-- [1,4,5]

til1_view = map I.view til1
-- ["1","(1 + 3)","(8 + (-(1 + 2)))"]

-- * //
-- Can we do the same for expressions represented in the final form?

tfl1 = [lit 1, add (lit 1) (lit 3), F.tf1]

tfl1_eval = map F.eval tfl1
-- [1,4,5]

tfl1_view = map F.view tfl1
-- ["1","(1 + 3)","(8 + (-(1 + 2)))"]

-- See the type of tfl1...

-- *Main> :t tfl1
-- tfl1 :: (ExpSYM repr) => [repr]

-- * //
-- Further questions about the final representation, in particular with
-- respect to pattern-match and expressing operations that are not fold
-- * How to pattern-match on terms?
-- *  How to process expressions unfold-like
-- * How to compare terms for equality?
-- * What if our language is higher-order?
-- * What if the language is typed?
-- (Incidentally, it is this question that has prompted our research.
-- That's where the label `tagless typed' comes from.)

main = do
       print til1_eval
       print til1_view
       print tfl1_eval
       print tfl1_view

--  LocalWords:  ExpSYM
 */