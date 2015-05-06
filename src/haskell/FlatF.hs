{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- * Demonstrating `non-compositional', context-sensitive processing
-- * The final style
-- * Flatten the additions

module FlatF where

import Intro2 hiding (main)
import PushNegF as Neg hiding (main,Ctx)

-- We are going to write flata as an interpreter.
-- After all, that's all we can do with an expression in a final form.

-- * The nested pattern-matching establishes a context:
-- * flata :: Exp -> Exp
-- * flata e@Lit{} = e
-- * flata e@Neg{} = e
-- * flata (Add (Add e1 e2) e3) = flata (Add e1 (Add e2 e3))
-- * flata (Add e1 e2)          = Add e1 (flata e2)

-- The processing is slightly different from push_neg, because we repeatedly
-- process the transformed expression (the last-but one clause)
-- The nested pattern-match again betrays the context-sensitivity.
-- The context in question is expression being the left child of addition.

-- * //
-- So, we define the context
-- (LCA e) means that the context (Add [] e), being the left immediate
-- child of addition whose right child is e. NonLCA is any other context.
data Ctx e = LCA e | NonLCA
-- On other words, LCA e3 represents the context of adding e3 _to the right_ of
-- the expression in focus.

instance ExpSYM repr => ExpSYM (Ctx repr -> repr) where
    lit n NonLCA   = lit n
    lit n (LCA e)  = add (lit n) e
    neg e NonLCA   = neg (e NonLCA)
    neg e (LCA e3) = add (neg (e NonLCA)) e3 -- assume only lits are negated
    add e1 e2 ctx  = e1 (LCA (e2 ctx))

-- The last clause expresses the reassociation-to-the-right
-- *	C[Add e1 e2] -> Add e1 C[e2]
-- Recall that after the negations are pushed down, expressions are
-- described by the following grammar
-- *    e ::= factor | add e e
-- *    factor ::= int | neg int
-- That is, C ::= [] | Add [] e

-- Keep in mind the processing is done bottom-up!

-- A general approach is to use continuations (or monads), 
-- or to encode the Ctx with zippers 
-- (the defunctionalized continuations, in the present
-- case).
-- The Ctx data type above is tuned to the present example,
-- keeping only the data we need (e.g., we aren't interested
-- in the context of Neg or of the addition on the left, 
-- and so we don't represent these cases in Ctx).


-- Exercise: there are several clauses where lit, neg, or add
-- appears on both sides of the definition. 
-- In which clause add is being used recursively?


-- The `interpreter' for flattening
flata :: (Ctx repr -> repr) -> repr
flata e = e NonLCA

--norm :: (Neg.Ctx -> Ctx c -> c) -> c
norm = flata . Neg.push_neg

-- Use our sample term
-- We make it a bit complex
tf3 = (add tf1 (neg (neg tf1)))

tf3_view = view tf3
-- "((8 + (-(1 + 2))) + (-(-(8 + (-(1 + 2))))))"

tf3_eval = eval tf3
-- 10

-- The normalized expression can be evaluated with any interpreter

tf3_norm = norm tf3

tf3_norm_view = view tf3_norm
-- "(8 + ((-1) + ((-2) + (8 + ((-1) + (-2))))))"

-- The result of the standard evaluation (the `meaning') is preserved

tf3_norm_eval = eval tf3_norm
-- 10

tf4 = add t (neg t) where t = (add (add (lit 1) (lit 2)) (lit 3))

tf4_view = view tf4
-- "(((1 + 2) + 3) + (-((1 + 2) + 3)))"

tf4_eval = eval tf4
-- 0

tf4_norm = norm tf4

tf4_norm_view = view tf4_norm
-- "(1 + (2 + (3 + ((-1) + ((-2) + (-3))))))"

-- The result of the standard evaluation (the `meaning') is preserved

tf4_norm_eval = eval tf4_norm
-- 0

main = do
       print tf3_view
       print tf3_eval
       print tf3_norm_view
       print tf3_norm_eval
       if tf3_eval == tf3_norm_eval then return ()
	  else error "Normalization"
       -- normalizing a normal form does not change it
       if (view . norm $ tf3_norm) == tf3_norm_view then return ()
	  else error "Normalization"
       print tf4_view
       print tf4_eval
       print tf4_norm_view
       print tf4_norm_eval
       if tf4_eval == tf4_norm_eval then return ()
	  else error "Normalization"
       if (view . norm $ tf4_norm) == tf4_norm_view then return ()
	  else error "Normalization"

