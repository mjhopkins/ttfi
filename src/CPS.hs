{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- CPS Transformation in tagless-final style
module CPS where

import TTF hiding (main)

-- * Parameterizing the arrows by the interpreter

-- *  CBAny.hs: type Arr exp a b = exp a -> exp b
-- That is different from the R interpreter we did earlier.
-- Evaluating an object term rep (Arr a b) in CBAny
-- would give us a function, but not of the type a -> b.
-- In the case of R, it would be R a -> R b.
-- It didn't matter for CBAny: We can't observe functional 
-- values anyway. In contrast, the term (repr Int) was interpreted 
-- in CBAny as Haskell Int, which we can observe. 
-- Thus, CBAny is suitable for encoding ``the whole code'' (a typical program),
-- whose result is something we can observe (but not apply).

-- Can we be flexible and permit interpretations of
-- repr (a->b) as truly Haskell functions a->b?
-- We need to make the interpretations of arrows dependent
-- on a particular interpreter.

-- * Emulating ML modules in Haskell
-- Making arrows dependent on the interpreter looks better in OCaml: 
-- we write a signature that contains the type ('a,'b) arrow.
-- Different structures implementing the signature will specify
-- the concrete type for ('a,'b) arrow.
-- To emulate this facility -- structures (modules) containing not 
-- only values but also types -- we need type functions.
-- We can use multi-parameter type classes with functional dependencies.
-- It seems more elegant here to use type families.


-- * //
-- * CBV CPS transform
-- *  CPS[ value ] = \k -> k $ CPSV[ value ]
-- *  CPS[ e1 e2 ] =
-- *   \k ->
-- *     CPS[ e1 ] (\v1 ->
-- *       CPS[ e2 ] (\v2 ->
-- *         v1 v2 k))
-- * (similar for addition)
-- *
-- *  CPSV[ basec ] = basec
-- *  CPSV[ x ]     = x
-- *  CPSV[ \x.e ]  = \x -> CPS[ e ]
-- We chose left-to-right evaluation order
-- * Danvy: CPS is *the* canonical transform
-- * CPS on types is NOT the identity
-- Why? Try to work out the types first

-- * //
newtype CPS repr w a = 
   CPS{cpsr :: repr ((CPSTypeTr w a -> w) -> w)}

-- * Define how to interpret arrow types in the CPS interpreter
-- (We could have used closed type families)
type family CPSTypeTr (w :: *) (a :: *) :: *
type instance CPSTypeTr w Int    = Int
type instance CPSTypeTr w Bool   = Bool
type instance CPSTypeTr w Falsum = Falsum
type instance CPSTypeTr w (a->b) = CPSTypeTr w a -> (CPSTypeTr w b -> w) -> w

-- Here, w is the answer-type
-- We observe the similarity with the double negation
-- CPS is the transform: we use the arrows of the base interpreter

data Falsum -- no constructors!
-- * Why Falsum is a better choice for the answer type than w?

-- Helpful auxiliary functions
cpsk :: Symantics repr =>
        (repr (CPSTypeTr w a -> w) -> repr w) -> CPS repr w a
cpsk = CPS . lam

appk :: Symantics repr =>
        CPS repr w a -> (repr (CPSTypeTr w a) -> repr w) -> repr w
appk (CPS e) f = app e $ lam f

-- * CPS of a value
cpsv :: Symantics repr => repr (CPSTypeTr w a) -> CPS repr w a
cpsv v = CPS . lam $ \k -> app k v

instance Symantics repr => Symantics (CPS repr w) where
    int x = cpsv $ int x
    add e1 e2 = cpsk $ \k ->
 		  appk e1 $ \v1 ->
 		  appk e2 $ \v2 ->
 		     app k (add v1 v2)
    lam e = cpsv $ lam (\x -> cpsr $ e (cpsv x))
    app ef ea = cpsk $ \k ->
 		  appk ef $ \vf ->
 		  appk ea $ \va ->
 		     app (app vf va) k


-- * //
-- * Applying the transform, evaluate afterwards

te1 = th1 -- re-interpreting the old Symantics term th1
-- te1 = add (int 1) (int 2)
-- te1 :: (Symantics repr) => repr Int

tec1 = cpsr te1
-- tec1 :: Symantics repr => repr ((Int -> w) -> w)

-- We need to pass the identity continuation
tec1_eval = eval tec1 id
-- 3

-- * The case of administrative redices
tec1_view = view tec1
-- "(\\x0 -> ((\\x1 -> (x1 1)) 
--    (\\x1 -> ((\\x2 -> (x2 2)) (\\x2 -> (x0 (x1+x2)))))))"

-- The result is a bit of a mess: lots of administrative redices

te2 = th2
-- te2 = lam (\x -> add x x)
-- te2 :: Symantics repr => repr (Int -> Int)

tec2 = cpsr te2
-- tec2 :: Symantics repr => repr (((Int -> (Int -> w) -> w) -> w) -> w)
tec2_eval = eval tec2
-- Try to apply id: eval tec2 id
-- and see the problem

-- The interpretation of a function is not usable, because of w...
-- Here we really need the answer-type polymorphism
-- OTH, like in CBAny.hs, the transform of a generally 'effectful'
-- function can be used in a `pure' code.
-- We can get a pure function out of tec2_eval; but
-- we have to do differently (from passing an identity continuation)
tec2_eval' = \a -> tec2_eval (\k -> k a id)
-- tec2_eval' :: Int -> Int

tec2_eval'' = tec2_eval' 21
-- 42

tec2_view = view tec2
-- even bigger mess

te3 = th3
-- te3 = lam (\x -> (app x (int 1)) `add` (int 2))
-- te3 :: Symantics repr => repr ((Int -> Int) -> Int)

te4 = let c3 = lam (\f -> lam (\x -> f `app` (f `app` (f `app` x))))
      in (c3 `app` (lam (\x -> x `add` int 14))) `app` (int 0)
-- * What is the type of te4?
-- te4 :: Symantics repr => repr Int

tec4 = cpsr te4
-- tec4 :: Symantics repr => repr ((Int -> w) -> w)

tec4_eval = eval tec4 id
-- 42

tec4_view = view tec4
-- view is a mess... makes a good wall-paper pattern though...

-- * //
-- * Composing interpreters: doing CPS twice
-- We have already seen how to chain tagless final interpreters.
-- We push this further: we do CPS twice

-- Generally, we need two answer-types, and so tecc1 will have the
-- following signature. Alas, the ambiguity check does not like it.
{-
tecc1 :: forall repr w2 w1.  Symantics repr =>
         repr ((((Int -> (CPSTypeTr w2 w1 -> w2) -> w2)
                 -> (CPSTypeTr w2 w1 -> w2) -> w2)
                -> w2) -> w2)
-}
{-
tecc1 :: Symantics repr =>
     repr ((((Int -> (Falsum -> w) -> w) -> (Falsum -> w) -> w) -> w)
           -> w)
-}
tecc1 = cpsr (tec1 :: Symantics repr => repr ((Int -> Falsum) -> Falsum))

-- The type makes it clear we did CPS twice


-- To evaluate the doubly-CPSed term, we have to do
-- more than just apply the identity continuation
-- flip($) is \v\k -> k v, which is sort of cpsv
tecc1_eval = eval tecc1
-- * If we set the answer type to Falsum, how ever we can get the result?
-- (See my article on undelimited continuations)
-- tecc1_eval' = tecc1_eval (\k -> k (flip ($)) id)
-- 3

tecc1_view = view tecc1
-- very big mess


-- * //
-- * One-pass CPS transform
-- Taking advantage of the metalanguage to get rid of the
-- administrative redices

newtype CPS1 repr w a = 
   CPS1{cps1r :: (repr (CPSTypeTr w a) -> repr w) -> repr w}

reflect :: Symantics repr =>
           ((repr a -> repr w) -> repr w) -> repr ((a -> w) -> w)
reflect e = lam (\k -> e (\v -> app k v))
-- * reflect e = lam (e . app)

-- * CPS1 of a value
cps1v :: repr (CPSTypeTr w a) -> CPS1 repr w a
cps1v v = CPS1 $ \k -> k v

-- The CPS1 instance looks quite like the CPS instance
instance Symantics repr => Symantics (CPS1 repr w) where
    int x = cps1v $ int x
    add e1 e2 = CPS1 $ \k ->
 		  cps1r e1 $ \v1 ->
 		  cps1r e2 $ \v2 ->
 		     k (add v1 v2)

    lam e = cps1v $ lam $ reflect . cps1r . e . cps1v

    app ef ea = CPS1 $ \k ->
 		  cps1r ef $ \vf ->
 		  cps1r ea $ \va ->
 		     app (app vf va) (lam k)

cps1 :: Symantics repr =>
        CPS1 repr w a -> repr ((CPSTypeTr w a -> w) -> w)
cps1 = reflect . cps1r

-- * //
-- * Applying the transform, evaluate afterwards

tek1 = cps1 te1
-- tek1 :: Symantics repr => repr ((Int -> w) -> w)

-- We need to pass the identity continuation
tek1_eval = eval tek1 id
-- 3

-- * The result is indeed much nicer
-- Administrative redices are gone, serious operations
-- (like addition) remain
tek1_view = view tek1
-- "(\\x0 -> (x0 (1+2)))"

tek2 = cps1 te2
-- tek2 :: Symantics repr => repr (((Int -> (Int -> w) -> w) -> w) -> w)

tek2_eval = eval tek2
tek2_eval'' = tek2_eval (\k -> k 21 id)
-- 42

-- Again, only serious redices remain
tek2_view = view tek2
-- "(\\x0 -> (x0 (\\x1 -> (\\x2 -> (x2 (x1+x1))))))"

tek4 = cps1 te4
tek4_eval = eval tek4 id
-- 42

tek4_view = view tek4
-- The result is large, but comprehensible...
-- "(\\x0 -> 
--   (((\\x1 -> (\\x2 -> (x2 (\\x3 -> (\\x4 -> ((x1 x3) (\\x5 -> ((x1 x5) 
--                         (\\x6 -> ((x1 x6) (\\x7 -> (x4 x7)))))))))))) 
--     (\\x1 -> (\\x2 -> (x2 (x1+14))))) 
--   (\\x1 -> ((x1 0) (\\x2 -> (x0 x2))))))"

-- * //
-- * Composing interpreters: doing CPS twice
-- We have already seen how to chain tagless final interpreters.
-- We push this further: we do CPS twice

tekk1 = cps1 (tek1 :: Symantics repr => repr ((Int -> Falsum) -> Falsum))
-- The type makes it clear we did CPS twice

tekk1_eval = eval tekk1

-- tekk1_eval ::
--   (((Int -> (w1 ->w) -> w) -> (w1->w) -> w) -> w) -> w
-- tekk1_eval' = tekk1_eval (\k -> k (flip ($)) id)
-- 3

tekk1_view = view tekk1
-- "(\\x0 -> (x0 (\\x1 -> (\\x2 -> ((x1 (1+2)) (\\x3 -> (x2 x3)))))))"
-- Can be eta-reduced
-- "(\\x0 -> (x0 (\\x1 -> (\\x2 -> ((x1 (1+2)) x2)))))"
-- "(\\x0 -> (x0 (\\x1 -> (x1 (1+2)) )))"

-- * Lessons
-- * The output of CPS is assuredly typed
-- * The conversion is patently total
-- * Composable transformers in the tagless final style


main = do
       print tec1_eval
       print tec1_view
       print tec2_eval''
       print tec2_view
       print tec4_eval
       print tec4_view

       print tecc1_view
       -- print tecc1_eval'

       print tek1_eval
       print tek1_view
       print tek2_eval''
       print tek2_view
       print tek4_eval
       print tek4_view

       -- print tekk1_eval'
       print tekk1_view

