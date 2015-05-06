{-# LANGUAGE TypeFamilies #-}

-- Converting from the De-Bruijn--based Symantics to the one based on the
-- higher-order abstract syntax.

module TTFdBHO where

import qualified TTF as HO              -- HOAS-based Symantics
import TTFdB hiding (main)              -- De Bruijn Symantics

newtype HORepr repr h a = HORepr (Hyp repr h -> repr a)

-- Build the environment for open terms (into which de Bruijn indices
-- index)
type family Hyp (repr :: * -> *) (a :: *) :: *
type instance Hyp repr () = ()
type instance Hyp repr (a,h) = (repr a, Hyp repr h)

-- De Bruijn-based interpreter in terms (HORepr repr), where repr is the
-- HOAS-based Symantics.

instance HO.Symantics repr => Symantics (HORepr repr) where
  int x = HORepr $ \_ -> HO.int x
  add (HORepr x) (HORepr y) = HORepr $ \h -> HO.add (x h) (y h)
  z = HORepr fst
  s (HORepr x) = HORepr $ \h -> x (snd h)
  app (HORepr x) (HORepr y) = HORepr $ \h -> HO.app (x h) (y h)
  lam (HORepr f) = HORepr $ \h -> HO.lam (\x -> f (x,h))

-- Interpret the tests from TTFdB, but using the HOAs-based interpreter

runHO :: HO.Symantics repr => HORepr repr () a -> repr a
runHO (HORepr m) = m ()

td1_hoeval = HO.eval . runHO $ td1
-- 3

td2_hoeval = HO.eval (runHO td2) 21
-- 42


td1_hoview = HO.view . runHO $ td1
-- "(1+2)"

td2_hoview = HO.view . runHO $ td2
-- "(\\x0 -> (x0+x0))"

td3_hoview = HO.view . runHO $ td3
-- "(\\x0 -> ((x0 1)+2))"


main = do
       print td1_hoeval
       print td2_hoeval
       print td1_hoview
       print td2_hoview
       print td3_hoview

