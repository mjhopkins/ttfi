{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Tagless-final 1-CPS translation for lambda-LG

{-
Our starting point is the regular CBV CPS translation for
lambda-LG, described on p 697 of the paper
   Michael Moortgat: Symmetric Categorial Grammar. J.Philos.Logic, 2009.
The translation follows the following pattern:

 A term of type A is translated as a computation of type
 (Tr(A) -> R) -> R, where R is a response type.
 A context of type A is translated as a continuation
 of type (Tr(A) -> R)
 A command is translated as a target term of type R.
 The translation of types is as follows:
  Tr(A) = A, for A primitive
  Tr(B/A) = Tr(A\B) = (Tr(B) -> R) -> (TR(A) -> R)
  Tr(B oleft A) = Tr(A oright B) = Tr(A\B) -> R

Strictly speaking, computations should be polymorphic
over R (or, we should chose the type R that is unpopulated/abstract).

We use HOAS for binding forms such mu and lambda, and their duals.

The original translation (eq (20) of the paper) yields many
administrative beta redices, which we would like to avoid. Therefore,
we use lightweight staging so to remove administrative redices as we
build the translated term.
-}

module LG where

import Text.PrettyPrint


-- The translation of types, from the Symmetric Categorial Grammar paper

data T					-- Two base symantic types
data E					-- individuals and propositions
data R					-- Response type

type NEG a = a -> R

data a :/ b				-- co-arrow

type family TR a :: *
type instance TR T = T
type instance TR E = E
type instance TR (a->b) = NEG (TR b) -> NEG (TR a)
type instance TR (a:/b) = NEG (NEG (TR b) -> NEG (TR a))


-- ------------------------------------------------------------------------
-- Tests and examples

-- mu g. <y | x \ g>
test1 = tlam "alpha0" $ \alpha0 ->
	tlam "x" $ \x ->
	tlam "y" $ \y -> 
	 top_c alpha0 (mu (\gamma -> (prim y) `cut` (prim x \\ gamma)))

{- Inferred type

*LG> :t test1
test1
  :: (TLC texp) =>
     texp ((a1 -> R) -> a -> ((a1 -> R) -> a -> R) -> R)

The result of the translation:

*LG> pp test1
\alpha0 -> \x_1 -> \y_2 -> y_2 alpha0 x_1
-}

-- The boy left, the first derivation
boyleft1 = 
 mu (\alpha0 -> prim left `cut`
      ((mu (\beta0 -> prim the `cut`
                        (beta0 // (mu (\gamma0 -> (prim boy) `cut` gamma0)))))
       \\ alpha0))
{-
*LG> :t boyleft1
boyleft1 :: (TConst texp, TLC texp) => RC texp T -> texp R
-}


testbr1 = top boyleft1
{-
*LG> :t testbr1
testbr1 :: (TConst texp, TLC texp) => texp ((T -> R) -> R)
*LG> pp testbr1
\alpha0 -> the (left alpha0) boy
-}

-- The boy left, second derivation
boyleft2 = 
 mu (\alpha0 -> prim the `cut`
   ((comu (\y0 -> prim left `cut` (y0 \\ alpha0))) //
    (mu   (\gamma0 -> prim boy `cut` gamma0))))

testbr2 = top boyleft2

-- Here we observe the eta-redex discussed in the title comments of LG.ml
{-
*LG> :t testbr2
testbr2 :: (TConst texp, TLC texp) => texp ((T -> R) -> R)
*LG> pp testbr2
\alpha0 -> the (\t_1 -> left alpha0 t_1) boy
-}

-- Molly teases the boy
mollyboy1 = 
 mu (\alpha0 -> prim teases `cut`
 (((mu (\beta0 -> prim molly `cut` beta0)) \\ alpha0) //
  (mu (\gamma0 -> prim the `cut`
                     (gamma0 // (mu (\alpha1 -> prim boy `cut` alpha1)))))))

{-
*LG> :t mollyboy1
mollyboy1 :: (TConst texp, TLC texp) => RC texp T -> texp R
-}

testmb1 = top mollyboy1
{-
*LG> :t testmb1
testmb1 :: (TConst texp, TLC texp) => texp ((T -> R) -> R)
*LG> pp testmb1
\alpha0 -> the (teases (\t_1 -> t_1 alpha0 molly)) boy
-}

mollyboy2 = 
 mu (\alpha0 -> prim the `cut`
 ((comu (\y0 -> prim teases `cut`
           (((mu (\gamma0 -> prim molly `cut` gamma0)) \\ alpha0) // y0))) //
  (mu (\alpha1 -> prim boy `cut` alpha1))))

testmb2 = top mollyboy2

{-
*LG> :t testmb2
testmb2 :: (TConst texp, TLC texp) => texp ((T -> R) -> R)
*LG> pp testmb2
\alpha0 -> the (\t_1 -> teases (\t_2 -> t_2 alpha0 molly) t_1) boy
-}

someoneleft =
    mu (\alpha0 -> prim someone `cut`
          (colam (\beta0 z0 ->
            (minus (mu (\alpha1 -> prim left `cut` (z0 \\ alpha1))) alpha0) 
	     `cut` beta0)))
{-
*LG> :t someoneleft
someoneleft :: (TConst texp, TLC texp) => RC texp T -> texp R
-}

testsl1 = top someoneleft

{-
*LG> :t testsl1
testsl1 :: (TConst texp, TLC texp) => texp ((T -> R) -> R)
*LG> pp testsl1
\alpha0 -> someone (\beta_1 -> \x_2 -> beta_1 (\t_3 -> left (t_3 alpha0) x_2))
-}


-- ------------------------------------------------------------------------
-- Target calculus

-- The translation relates two languages: lambda-mu-mubar calculus
-- and the regular lambda-calculus with constants.
-- We first define the target calculus, call-by-value
-- lambda-calculus with constants.
-- We prefix the types and classes with the uppercase T, and terms
-- with the lower-case t to show we are talking about the target
-- calculus.

-- Variable names can be meaningful. We should strive to preserve
-- them when printing out the terms
type VarName = String

class TLC texp where
    tlam :: VarName -> (texp a -> texp b) -> texp (a->b)
    tapp :: texp (a->b) -> (texp a -> texp b)

-- The left-associative infix # operator is the synonym for tapp
infixl 8 #
f # x = tapp f x

-- Constants
-- The signature. Not very extensible at the moment, but we don't
-- need the extensibility for now

class TConst texp where
    tce    :: String -> texp (TR E)
    tcet   :: String -> texp (TR (E->T))
    tcete  :: String -> texp (TR ((E->T)->E))
    tcvt   :: String -> texp (TR (E -> E -> T)) -- transitive verb
    tqua   :: String -> texp (TR (E :/(T:/T)))

-- Sample lexical items. They are typed (the types are inferred)
left  = tcet "left"
molly = tce  "molly"
boy   = tcet "boy"
the   = tcete "the"
teases = tcvt "teases"
someone = tqua "someone"

-- Sample target term
tt_boyleft = tlam "alpha0" (\alpha0 -> the # (left # alpha0) # boy)

{- Inferred type:
*LG> :t tt_boyleft
tt_boyleft :: (TConst texp, TLC texp) => texp ((T -> R) -> R)
-}

{- Print the term:
*LG> pp tt_boyleft
\alpha0 -> the (left alpha0) boy
-}

-- ------------------------------------------------------------------------
-- Source calculus

-- Source calculus is defines in the tagless-final way, via the
-- `constructor functions' prim, mu, comu, lam, (//), (\\), etc.

-- Commands
cut v e = v e

-- Terms
-- The translation of a term is a computation, of the type:
type RT texp a = RC texp a -> texp R

prim:: TLC texp => texp a -> RT texp a
prim x = \k -> plug k x			-- primitives (e.g., lexical items)

-- Essentially, the identity
mu c = \alpha -> c alpha

minus :: (TLC texp) =>
     RT texp a -> RC texp b -> RT texp (((b -> R) -> a -> R) -> R)
minus v e = prim (tran_dy (v \\ e))

lam:: (TLC texp) =>
     (RC texp c -> RT texp a -> texp R) -> RT texp ((c -> R) -> a -> R)
lam c = prim (tlam "beta" $ \beta ->
	      tlam "x"    $ \x -> 
	       c (Exp beta) (prim x))


-- Contexts, of the type RC texp a

infix 7 \\, //
(\\):: TLC texp => RT texp a -> RC texp b -> RC texp ((b->R) -> (a->R))
v \\ e = Tra (\u -> v (Exp (tapp u (tran_dy e))))

e // v = v \\ e

comu :: TLC texp => (RT texp a -> texp R) -> RC texp a
comu c = Tra (\x -> c (prim x))

colam c = Tra (\u -> (lam c) (Exp u))



{-
 The translation of a context can now be (texp A -> texp R),
 or texp (A->R). We need both forms in different parts of
 the translation. Therefore, we define the following union type.
-}

data RC texp a = 			-- Representation of contexts
    Exp (texp (a -> R))			-- Finished expression
  | Tra (texp a -> texp R)		-- Expression transformer


-- The type of eta is (texp a -> texp b) -> texp (a -> b)
-- As its name implies, it may introduce an eta-redex.
-- It is easy to see that 'tapp' and 'eta' are inverses.

eta f = tlam "t" f

{-
   Here is the secret of avoiding eta-redices: keep track of the form
   for the representation of contexts, and converting between the two
   forms.
-}
tran_dy (Exp x) = x
tran_dy (Tra x) = eta x

-- plug a context with a term
plug :: TLC texp => RC texp a -> texp a -> texp R
plug (Exp kv) x = tapp kv x
plug (Tra f) x  = f x

-- To run the computation
top_c alpha v = v (Exp alpha)
top tr = tlam "alpha0" $ \alpha0 -> top_c alpha0 tr


-- ------------------------------------------------------------------------
-- Pretty-printing of the terms of TLC

data Precedence = NoPrec | OpPrec | AppPrec
		deriving (Eq, Ord, Enum)
data PPEnv = PPEnv{prec :: Precedence, nesting :: Int}

newtype PP a = PP{unPP:: PPEnv -> Doc}

instance TConst PP where
    tce   name = PP $ const $ text name
    tcet  name = PP $ const $ text name
    tcete name = PP $ const $ text name
    tcvt  name = PP $ const $ text name
    tqua  name = PP $ const $ text name

instance TLC PP where
    tapp (PP f) (PP x) = 
	PP $ \env -> parensIf (prec env >= AppPrec) $ 
		        f (env{prec=OpPrec}) <+> x (env{prec=AppPrec})
    tlam name f = PP $ \env -> 
      let nm = text $ name ++ (if nesting env == 0 then "" 
			          else "_"++show (nesting env))
          body = unPP (f (PP $ const nm)) (PPEnv{nesting = nesting env + 1,
						 prec = NoPrec})
      in parensIf (prec env > NoPrec) $ 
           char '\\' <> nm <+> text "->" <+> body


parensIf :: Bool -> Doc -> Doc
parensIf True d = parens d
parensIf False d = d


pp term = unPP term (PPEnv{prec=NoPrec, nesting=0})


