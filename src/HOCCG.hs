{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Non-associative Lambek calculus NL with the non-standard semantic
-- interpretation
-- Applications to quantification, non-canonical coordination and gapping

module HOCCG where

import Semantics
import Prelude hiding ((/), (\\), and)

-- Types 
-- directional implication types
data a :/ b
data a :\ b

-- Product type
--    usual Haskell product (a,b)
data O                                  -- empty sequence

data Neg a                              -- referent type (provided type)

-- base types
data NP
data S

-- Types for sentence structures (context markers)
data And
data VB                                 -- TV argument boundary
                                        -- (when TV was gapped)

-- Types used by quantifiers
data Un'
data Ex'

type Un = (Un', O) 
type Ex = (Ex', O) 

-- Convenient abbreviations
type VP = NP :\ S
type VT = VP :/ NP
type PP = VP :\ VP
type CN = NP :\ S
  
-- Rules of the calculus: a variant of the non-associative
-- Lambek calculus NL

-- It reminds of De Bruijn encoding of variables; the encoding is
-- more structured now.

class Symantics repr where
  (/)  :: repr g (b :/ a) -> repr d a -> repr (g,d) b
  (\\) :: repr g a -> repr d (a :\ b) -> repr (g,d) b
  inL  :: repr (a,d) b -> repr d (a :\ b)
  inR  :: repr (d,a) b -> repr d (b :/ a)
  var  :: repr a a

  -- The following cut-like
  -- is essentially the derived rule. We define it here for simplicity
  -- (and to be able to contract (O,O) to O, which cannot be done
  --  in other ways)
  hypS ::  repr (d :\ a) (g :\ a) -> repr d a -> repr g a

infixl 8 /,\\

-- Cut rules (derivable)
hypL :: Symantics repr => repr g a -> repr (a,d) b -> repr (g,d) b
hypL h f = h \\ inL f

hypR :: Symantics repr => repr (d,a) b -> repr g a -> repr (d,g) b
hypR f h = inR f / h

-- potential extension: classical-like sequent
--   Gamma |- Delta
-- where Gamma shows what's required and Delta what is provided


-- So-called structural rules
class Symantics repr => Structural repr where
  oo   :: repr ((O,O) :\ a)     (O :\ a)       -- contract (O,O) to O 
  oL   :: repr ((O,(O,g)) :\ a) ((O,g) :\ a)   -- contract sequences of O
  oR   :: repr (((g,O),O) :\ a) ((g,O) :\ a)

          -- apply the structural rule to the left component
  sL   :: repr (g1 :\ a) (g2 :\ a) -> repr ((g1,d) :\ a) ((g2,d) :\ a)
           -- or the right component
  sR   :: repr (g1 :\ a) (g2 :\ a) -> repr ((d,g1) :\ a) ((d,g2) :\ a)
  
          -- simplifying coordination contexts
  andC :: repr ((g,(And,g)) :\ S) (g :\ S)

  andL :: repr (( ((Neg a,b),g), (And, (a,d))) :\ S)  -- at the left edge
               (((b,g),(And,(O,d))) :\ S)
  andR :: repr (( (g,a), (And, (d,(Neg a,b)))) :\ S)  -- at the right edge
               (((g,O),(And,(d,b))) :\ S)
  -- perhaps it should be as follows, and then add the rule
  -- (VB,O) -> VB   (O,VB) -> VB
  -- Then we can add VB to andL and andR
  -- General principle: reference is replaced with a context
  -- marker, to keep the structure and prevent reassociation
  -- (using oL). We do need oL: otherwise, we can't apply hypR at all.
  -- andD :: repr                                        -- in the middle
  --           (((g1, ((Neg a, b), g2)), (And, (d1, (a, d2)))) :\ S)
  --           (((g1, (b, g2)), (And, (d1, (VB, d2)))) :\ S)
  andD :: repr                                        -- in the middle
            (((g1, ((Neg a, O), g2)), (And, (d1, (a, d2)))) :\ S)
            (((g1, (VB, g2)), (And, (d1, (VB, d2)))) :\ S)

-- general andD has restrictions: first, only for relations such as
-- TV; second, do not replace the Hyp with O but with some other type.
-- (to block jlab2_2). So, when we refer to a verb we can't also
-- refer to its arguments.

           -- Quantifier types can freely move through the
           -- antecedent structure
  ou   :: repr ((O,(Un,g)) :\ a) ((Un, (O,g)) :\ a)
  uc   :: repr ((O,Un) :\ a) ((Un,O) :\ a)
  -- Un can pass over Ex
  ucE  :: repr ((Ex,(Un,g)) :\ a) ((Un,(Ex,g)) :\ a)
  -- Ex can always float up
  ec   :: repr ((g,Ex) :\ a) ((Ex,g) :\ a)
  ecL  :: repr (((Ex,g),d) :\ a) ((Ex,(g,d)) :\ a)
  ecR  :: repr ((d,(Ex,g)) :\ a) ((Ex,(d,g)) :\ a)

-- Add the constant to discard context marker:
--  Ctx,nil -> Ctx. Define the type class ContextualMarker
-- and define this constant generically.

-- Contract (O,O) to O
ooT :: Structural repr => repr (O,O) a -> repr O a
ooT = hypS oo 

class Constants repr where
  -- Lexical entries
  and'                         :: repr And ((S :\ S) :/ S)
  john, mary, bill, sue        :: repr O NP
  chicago, detroit             :: repr O NP
  monday, tuesday              :: repr O NP
  tripped, fell, left, went    :: repr O VP
  like, saw, hate, bought      :: repr O VT
  book, record, gift           :: repr O CN
  gave        :: repr O ((VP :/ PP) :/ NP)
  on,to,in_   :: repr O (PP :/ NP)
  everyone    :: repr Un NP
  someone     :: repr Ex NP
  a           :: repr Ex' (NP :/ CN)

  -- auxiliaries (silent)
  qForall     :: repr O (S :/ (Un :\ S))
  qExists     :: repr O (S :/ (Ex :\ S))

  ref         :: repr (Neg a) (a :/ a)

infix 1 `and`
and x y = x \\ (and' / y)

-- ========================================================================
-- Examples

-- ------------------------------------------------------------------------
-- Quantification

jle = hypS oL $ john \\ (like / everyone)
-- repr (O, Un) S

jle_1 = hypS uc $ jle
-- repr (Un, O) S

jle_2 = ooT $ qForall / (inL jle_1)


jle_EN = runEN jle_2
-- "John liked everyone"
jle_sem = shsem . runSem $ jle_2
-- "A (Lx1. like' x1 john')"

slm = (someone \\ (like / mary))
-- repr (Ex, (O, O)) S

slm_1 = qExists / (inL slm)
-- repr (O, (O, O)) S

slm_2 = ooT $ hypS oL $ slm_1
slm_EN = unEN slm_2
-- "someone liked Mary"

slm_sem = shsem . runSem $ slm_2
-- "E (Lx1. like' mary' x1)"

-- quantifier ambiguity
sle = someone \\ (like / everyone)
-- repr (Ex, (O, Un)) S

sle1 = hypS oL $ qExists / (inL sle)
-- repr (O, Un) S

sle1_1 = ooT $ qForall / (inL (hypS uc sle1))

sle1_EN = unEN sle1_1
-- "someone liked everyone"

-- inverse reading
sle1_sem = shsem . runSem $ sle1_1
-- "A (Lx1. E (Lx2. like' x1 x2))"

sle2 = hypS (sR uc) $ sle
-- repr (Ex, (Un, O)) S

sle2_1 = hypS ucE $ sle2
-- repr (Un, (Ex, O)) S

sle2_2 = qForall / (inL sle2_1)
-- repr (O, (Ex, O)) S

sle2_3 = hypS ecR sle2_2
-- repr (Ex, (O, O)) S

sle2_4 = ooT . hypS oL $ qExists / (inL sle2_3)

sle2_EN = unEN sle2_4
-- "someone liked everyone"

sle2_sem = shsem . runSem $ sle2_4
-- "E (Lx1. A (Lx2. like' x2 x1))"

-- Non-trivial restrictor
mlab = hypS oL $ mary \\ (like / (a / book))
-- repr (O, Ex) S
mlab_1 = ooT $ qExists / (inL . hypS ec $ mlab)

mlab_EN = unEN mlab_1
-- "Mary liked a book"

mlab_sem = shsem . runSem $ mlab_1
-- "E (Lx1. book' x1 & like' x1 mary')"


-- ------------------------------------------------------------------------
-- Coordination

-- On one hand, we could use the polymorphic AND, to coordinate
-- any two values of the same type (category).
-- However, we demonstrate our hyp reasoning.

-- VP coordination

-- John tripped and fell

-- First analysis, using only hypotheses
jtaf1 = ooT $ john `hypL` (hypS andC (var \\ tripped `and` var \\ fell))

jtaf1_en = runEN jtaf1
-- "John tripped and fell"

jtaf1_sem = shsem . runSem $ jtaf1
-- "tripped' john' & fell' john'"

-- Intuitively, paraphrase: John tripped. He fell.
jtaf2_1 = ((ref / john) \\ tripped) `and`  var \\ fell
-- repr (((Neg NP, O), O), (And, (NP, O))) S

jtaf2_2 = hypS andL jtaf2_1
-- repr ((O, O), (And, (O, O))) S

jtaf2_3 = ooT $ hypS andC jtaf2_2
jtaf2_en = runEN jtaf2_3
-- "John tripped and fell"

jtaf2_sem = shsem . runSem $ jtaf2_3
-- "E! (Lx1. (tripped' john' & fell' x1) & (john' = x1))"

-- *John left and
jla_1 = (ref / john) \\ (ref / left) `and` (var \\ var)
-- repr (((Neg NP, O), ((Neg, NP :\ S), O)), (And, (a, a :\ S))) S

jla_2 = hypS andL jla_1
-- repr ((O, (Neg (NP :\ S), O)), (And, (O, NP :\ S))) S
-- And then we are stuck. We cannot apply andR (because Neg must be
-- on the right) and we cannot apply andL. We cannot apply the
-- arbitrary andD since it only works for relations (such as VT)
-- and VP is not.


-- Subject coordination
jaml1 = ooT $ (hypS andC ((john \\ var) `and` (mary \\ var))) `hypR` left

jaml1_EN = runEN jaml1
-- "John and Mary left"

jaml1_sem = shsem . runSem $ jaml1
-- "left' john' & left' mary'"

jaml2_1 = (john \\ var) `and` mary \\ (ref / left)
-- repr ((O, NP :\ S), (And, (O, (Neg (NP :\ S), O)))) S

jaml2_2 = hypS andR jaml2_1
-- repr ((O, O), (And, (O, O))) S

jaml2_3 = ooT $ hypS andC jaml2_2
jaml2_EN = runEN jaml2_3
-- "John and Mary left"

jaml2_sem = shsem . runSem $ jaml2_3
-- "E! (Lx1. (fst (x1 john') & left' mary') &
--           ((Lx2. (left' x2, T)) = x1) & snd (x1 john'))"


-- Object coordination

jlbm1_1 = hypS andC ((var \\ (var / bill)) `and` (var \\ (var / mary)))
-- repr (a, ((a :\ S) :/ NP, O)) S
jlbm1_2 = john `hypL` jlbm1_1
-- repr (O, ((NP :\ S) :/ NP, O)) S

-- But then we can't apply like, because it is no longer at the edge
-- jlbm1_3 = like `hypL` jlbm1_2

-- John liked Bill. (So) He did Mary.

jlbm2_1 = (ref / john) \\ ((ref / like) / bill) `and`
          (var \\ (var / mary))
-- repr (((Neg NP, O), ((Neg ((NP :\ S) :/ NP), O), O)),
--           (And, (a, ((a :\ S) :/ NP, O)))) S

jlbm2_2 = hypS andL jlbm2_1
-- repr ((O, ((Neg ((NP :\ S) :/ NP), O), O)),
--           (And, (O, ((NP :\ S) :/ NP, O)))) S

jlbm2_3 = hypS andC $ hypS andD jlbm2_2
-- repr (O, (VB, O)) S

-- VT and other relational things can be referred even in the middle,
-- without restriction!

{-
jhbm =  hyp it . pair $ \ (hpj,hj) -> hyp it . pair $ \ (hpl,hl) -> 
  (ref_john / john / hpj) \\ ((ref_liked / like / hpl) / bill) `and`
  (trace / hj) \\ ((trace / hl) / mary)

jhbm_EN = unEN jhbm
-- "John liked Bill and Mary"
jhbm_sem = shsem . unSem $ jhbm
-- "E! (Lx1. E! (Lx2. like' bill' john' & x2 mary' x1))"
-}

-- VT coordination (medial coordination)

jlhb1 = ooT $
        ((hypS oL $ john `hypL`
         (hypS andC ((var \\ (like / var)) `and` (var \\ (hate / var)))))
         `hypR` bill)

jlhb1_EN = runEN jlhb1
-- "John liked and hated Bill"

jlhb1_sem = shsem . runSem $ jlhb1


jlhb2_1 = (ref / john) \\ (like / var) `and`
          (var \\ (hate / (ref / bill)))
-- repr (((Neg NP, O), (O, NP)), (And, (NP, (O, (Neg NP, O))))) S

jlhb2_2 = hypS andL jlhb2_1
-- repr ((O, (O, NP)), (And, (O, (O, (Neg NP, O))))) S

jlhb2_3 :: (Structural repr, Constants repr) =>
     repr ( (O, NP), (And, (O, (Neg NP, O)) )) S
jlhb2_3 = undefined

jlhb2_4 = hypS andR jlhb2_3
-- repr ((O, O), (And, (O, O))) S


-- Test that we cannot derive *John liked Bill and hated

jlhb3_1 = (ref / john) \\ (like / (ref / bill)) `and`
          (var \\ (hate / var))
-- repr ((((Neg, NP), O), (O, ((Neg, NP), O))),
--         (And, (NP, (O, NP)))) S
          
jlhb3_2 = hypS andL jlhb3_1
-- repr ((O, (O, (Neg NP, O))), (And, (O, (O, NP)))) S

-- Then we are stuck since to apply andR, Neg must be on the right.

{-
jlhb = hyp it . pair $ (\ (hpb,hb) ->
       hyp it . pair $ (\ (hpj,hj) ->
  (ref_john / john / hpj) \\ (like / (trace / hb)) `and`
  (trace / hj) \\ (hate / (ref_bill / bill / hpb))))

ref_bill :: Constants repr => repr ((NP :/ (Sing SBill :\ S)) :/ NP)
ref_bill = ref
       
jlhb_sem = shsem . unSem $ jlhb
-- "E! (Lx1. E! (Lx2. like' x1 john' & hate' bill' x2))"
-}

-- (Mis)Examples of overgeneration 

-- *John likes and Bill
jlab1_1 = (hypS andC $ (var \\ (var / var)) `and` (var \\ (var / var)))
-- repr (a, ((a :\ S) :/ a1, a1)) S
-- We can apply john with hypL, but after that we are stuck.

jlab2_1 = ((ref / john) \\ ((ref / like) / var)) `and`
          (var \\ (var / (ref / bill)))

jlab2_2 = hypS andD $ hypS andL jlab2_1
-- repr ((O, (VB, NP)), (And, (O, (VB, (Neg NP, O))))) S

-- we can't apply hypR because NP is not the right node of the left
-- conjunct.

-- see the comment for andD...


-- Non-canonical coordination

-- John likes but Mary hates Bill.

jlmhb1_1 = hypS andC $ john \\ (like / var) `and` (mary \\ (hate / var))
-- jlmhb1 :: (Structural repr, Constants repr) => repr (O, (O, NP)) S

jlmhb1_2 = ooT $ hypS oL jlmhb1_1 `hypR` bill

jlmhb1_EN = runEN jlmhb1_2
-- "John liked and Mary hated Bill"

jlmhb1_sem = shsem . runSem $ jlmhb1_2
-- "like' bill' john' & hate' bill' mary'"

jlmhb2_1 = john \\ (like / var) `and` (mary \\ (hate / (ref / bill)))
-- repr ((O, (O, NP)), (And, (O, (O, ((Neg, NP), O))))) S


-- *John liked Bill and Mary hated __
-- is not derivable because to apply andR, the Neg must be on the right.

{-
-- Other interesting examples
-- John saw Bill in Detroit and Chicago
-- John saw Bill and Mary in Detroit and Chicago
-- (non-collective reading of Bill and Mary, meaning saw separately)
-- The delimited control will be handy there.

-- John went to Chicago on Monday and to Detroit on Tuesday
-- Paraphrase: John went to Chicago on Monday. He did to Detroit on Tuesday.

jwtcomtd_EN = unEN jwtcomtd
-- "John went to Chicago on Monday and to Detroit on Tuesday"
jwtcomtd_sem = shsem . unSem $ jwtcomtd
-- "on' monday' (to' chicago' went') john' &
--  on' tuesday' (to' detroit' went') john'"

-- raising out 'to' is trivial
-- John went to Chicago on Monday and Detroit on Tuesday
-- Paraphrase: John went to Chicago on Monday. He did hto Detroit on Tuesday

jwtcom_EN = unEN jwtcom
-- "John went to Chicago on Monday and Detroit on Tuesday"
jwtcom_sem = shsem . unSem $ jwtcom
-- "on' monday' (to' chicago' went') john' &
--  on' tuesday' (to' detroit' went') john'"

-}

-- Gapping
-- Mary bought a CD and John a book
-- Mary liked Chicago and Bill Detroit

mldajc = hypS andC $ hypS andD $
  mary \\ ((ref / like) / chicago) `and`
  bill \\ (var / detroit)
-- repr (O, (VB, O)) S

{-
mldajc_EN = unEN mldajc
-- "Mary liked Chicago and Bill Detroit"
mldajc_sem = shsem . unSem $ mldajc
-- "E! (Lx1. like' chicago' mary' & x1 detroit' bill')"

-- (KubotaLevine, ESSLLI 13)
-- No man sang and danced
--   (a) No man sang and no man danced
--   (b) There was no man who both sang and danced
-}

-- (KubotaLevine, ESSLLI 13, HybridGrammar)
-- I gave a present to Robin on Thursday and to Leslie on
-- Friday.
-- During the past month, a mob boss was assassinated in
-- Boston and was executed in New York.
-- (here, only narrow scope is semantically meaningful)

gap = gave / (a / gift)
--  repr (O, Ex) (VP :/ PP)

jgpb = john \\ ((gap / (to / bill)) \\ (on / monday))
-- repr (O, (((O, Ex), (O, O)), (O, O))) S

-- Wide scope of "a present"
jgpb1 =
  (ref / john) \\ (((ref / gap) / (to / bill)) \\ (on / monday))
  `and`
  var \\ ((var / (to / mary)) \\ (on / tuesday))

-- repr (((Neg NP, O), (((Neg (VP :/ PP), (O, Ex)), (O, O)), (O, O))),
--   (And, (NP,        ((VP :/ PP, (O, O)), (O, O))))) S

jgpb1_1 = hypS andL jgpb1
-- repr ((O, (((Neg (VP :/ PP), (O, Ex)), (O, O)), (O, O))),
-- (And, (O, ((VP :/ PP, (O, O)), (O, O))))) S

-- jgpb1_2 = hypS andD jgpb1_1


-- Narrow scope of "a present"

gap2 = inL $ hypS ec gap
-- repr O (Ex :\ (VP :/ PP))

jgpb2A =
  (ref / john) \\ (((var \\ (ref / gap2)) / (to / bill)) \\ (on / monday))
-- repr ((Neg NP, O),
--           (((Ex, (Neg (Ex :\ (VP :/ PP)), O)), (O, O)), (O, O))) S

jgpb2A_1 = hypS ecR . hypS (sR $ ecL) .  hypS (sR . sL $ ecL) $  jgpb2A
-- repr (Ex,
--     ((Neg NP, O), (((Neg (Ex :\ (VP :/ PP)), O), (O, O)), (O, O)))) S

jgpb2A_2 = qExists / inL jgpb2A_1
-- repr (O,
--  ((Neg NP, O), (((Neg (Ex :\ (VP :/ PP)), O), (O, O)), (O, O)))) S

jgpb2B =
  var \\ (((var \\ var) / (to / mary)) \\ (on / tuesday))
-- repr (NP, (((a, a :\ (VP :/ PP)), (O, O)), (O, O))) S

jgpb2B_1 = hypS ecR . hypS (sR $ ecL) .  hypS (sR . sL $ ecL) $  jgpb2B
-- repr (Ex, (NP, ((Ex :\ (VP :/ PP), (O, O)), (O, O)))) S

jgpb2B_2 = qExists / inL jgpb2B_1
-- repr (O, (NP, ((Ex :\ (VP :/ PP), (O, O)), (O, O)))) S


{-
jgpb1 =
  (ref / john) \\ (((ref / gap) / (to / bill)) \\ (on / monday))
  `and`
  var \\ ((var / (to / mary)) \\ (on / tuesday))

jgp2 = hyp it . pair $ (\ (hpj,hj) ->
       hyp it . pair $ (\ (hpg,hg) ->
       hyp it . pair $ (\ (hpp,hp) ->
   hyp qExistS (\qe ->
   (ref_john / john / hpj) \\
   (((ref_gave / gave / hpg) / 
      (ref_gift / aGift / hpp / qe) / (to / bill)) \\ (on / monday))
   `and`
   hyp qExistS (\qe ->
   (trace / hj) \\
   (((trace / hg) / 
      (trace / hp / qe) / (to / mary)) \\ (on / tuesday))
   )))))

jgp2_EN = unEN jgp2
-- "John gave a present to Bill on Monday and to Mary on Tuesday"
jgp2_sem = shsem . unSem $ jgp2
-- "E! (Lx1. E! (Lx2. E! (Lx3.
--  E! (Lx4. on' monday' (to' bill' (give' x4)) john' &
--  E! (Lx5. on' tuesday' (x2 (x3 x5) (to' mary')) x1)))))"

-- "E (Lx1. present' x1 & on' monday' (to' bill' (give' x1)) john') &
--  E (Lx1. present' x1 & on' tuesday' (to' mary' (give' x1)) john')"


-- Symmetrical (KubotaLevine, ESSLLI 13)
-- (16) a. Chris told the same joke to Robin on Wednesday and (to) Leslie on
-- Saturday
-- John likes but Mary hates the same guy.
-- Robin was singing, and Leslie was whistling, the same tune.
-- Start with: the same waiter served John and Bill

-- Summative (KubotaLevine, ESSLLI 13)
-- John gave a total of $10,000 to Mary on Tuesday and to Sue
-- on Saturday.

-- John spent and Mary lost the total of $10,000.
--  Here, $10,000 is the _sum_ of John's spendings and Mary's losses.

-- Quantifiers and gapping (KubotaLevine, ESSLLI 13)
-- Chris said something to Leslie, and Terry to Robin.

-- Mrs. J mustn't live in LA and Mr. J. in Boston.
-- (from [Oehrle, 1987]).
-}

-- ------------------------------------------------------------------------
-- Phonetic interpretation
-- Displaying the yield (surface string)
newtype EN g a = EN{unEN :: String}

instance Symantics EN where
  EN x /  EN y  = EN $ ccat x y
  EN x \\ EN y  = EN $ ccat x y
  inL (EN x)    = EN x
  inR (EN x)    = EN x
  -- Hypotheses are phonetically silent
  hypS _ (EN m) = EN m
  var           = EN ""

ccat :: String -> String -> String
ccat "" x = x
ccat x "" = x
ccat x y = x ++ " " ++ y

instance Constants EN where
  and'    = EN "and"
  john    = EN "John"
  mary    = EN "Mary"
  bill    = EN "Bill"
  sue     = EN "Sue"
  chicago = EN "Chicago"
  detroit = EN "Detroit"
  monday  = EN "Monday"
  tuesday = EN "Tuesday"
  tripped = EN "tripped"
  fell    = EN "fell"
  left    = EN "left"
  went    = EN "went"
  like    = EN "liked"
  saw     = EN "saw"
  bought  = EN "bought"
  book    = EN "book"
  record  = EN "record"
  gift    = EN "gift"
  hate    = EN "hated"
  gave    = EN "gave"
  on      = EN "on"
  to      = EN "to"

  everyone = EN "everyone"
  someone  = EN "someone"
  a        = EN "a"

  -- Silent terms
  qForall  = EN ""
  qExists  = EN ""

  ref      = EN ""

-- phonetically, the structural rules are silent
instance Structural EN where
  oo   = EN ""
  oL   = EN ""
  oR   = EN ""
  sR _ = EN ""
  andC = EN ""
  andL = EN ""
  andR = EN ""
  andD = EN ""
  ou   = EN ""
  uc   = EN ""
  
runEN :: EN O S -> String
runEN = unEN

-- ------------------------------------------------------------------------
-- Semantic interpretation

newtype Sem l env a = Sem{unSem:: l (Tr (env :\ a))}

-- Translating types and environments
type family Tr a :: *
type instance Tr NP  = Entity
type instance Tr S   = Bool
type instance Tr O   = ()
type instance Tr Un' = Entity
type instance Tr Ex' = Entity
type instance Tr (b :/ a) = Tr a -> (Tr b, Bool)
type instance Tr (a :\ b) = Tr a -> (Tr b, Bool)
type instance Tr (a,b) = (Tr a,Tr b)

-- Context markers have no semantic significance
type instance Tr And = ()

-- Neg types are actually the same as x....
type instance Tr (Neg x) = Tr x

-- Adding pairs to Lambda, the language of Semantics (FOL)
class Lambda l => LambdaPair l where
  pair :: l a -> l b -> l (a,b)
  car  :: l (a,b) -> l a
  cdr  :: l (a,b) -> l b
  unit :: l ()

-- Lam with no side conditions
lamP :: LambdaPair lrepr => (lrepr a -> lrepr b) -> lrepr (a -> (b,Bool))
lamP body = lam $ \x -> pair (body x) true

instance LambdaPair l => Symantics (Sem l) where
  Sem f / Sem x = Sem . lam $
     \gd -> let fv = app f (car gd)
                xv = app x (cdr gd)
                bv = app  (car fv) (car xv)
            in pair (car bv)
                    (conj (cdr bv) (conj (cdr fv) (cdr xv)))
  Sem x \\ Sem f = Sem . lam $
     \gd -> let fv = app f (cdr gd)
                xv = app x (car gd)
                bv = app  (car fv) (car xv)
            in pair (car bv)
                    (conj (cdr bv) (conj (cdr fv) (cdr xv)))
  inL (Sem f) = Sem . lamP $
     \d ->  lam $ \a -> app f (pair a d)
  inR (Sem f) = Sem . lamP $
     \d ->  lam $ \a -> app f (pair d a)

  var  = Sem . lamP $ id

  hypS (Sem tr) (Sem x) = Sem . lam $
     \g -> let tv = app tr (lam $ \d -> app x d)
               r  = app (car tv) g
           in pair (car r) (conj (cdr tv) (cdr r))
  

runSem :: LambdaPair l => Sem l O S -> l Bool
runSem (Sem x) = let r = app x unit in conj (car r) (cdr r)


instance (Lam1 l, LambdaPair l) => Structural (Sem l) where
  oo   = Sem . lamP $
     \ga -> lam $ \d -> app ga (pair d d)

  oL   = Sem . lamP $
     \ga -> lam $ \d -> app ga (pair unit d)

  sR (Sem r)  = Sem . lamP $
     \dg1a -> lam $ \dg2 ->
      let rv = app r (lam $ \g1 -> app dg1a (pair (car dg2) g1))
          av = app (car rv) (cdr dg2)
      in (pair (car av) (conj (cdr rv) (cdr av)))

  andC = Sem . lamP $
     \ga -> lam $ \d -> app ga (pair d (pair unit d))

  andL = Sem . lamP $
     \ga -> lamP $ \bgAod ->
                let b  = car . car $ bgAod
                    g  = cdr . car $ bgAod
                    d  = cdr . cdr . cdr $ bgAod
                    r x =
                      let rv =
                           app ga ( ((pair x b) `pair` g) `pair`
                                    (unit `pair` (pair x d)))
                      in conj (car rv) (cdr rv)
                in
                app gexists $ lam r

  andR = Sem . lamP $
     \ga -> lamP $ \goAdb ->
                let g  = car . car $ goAdb
                    d  = car . cdr . cdr $ goAdb
                    b  = cdr . cdr . cdr $ goAdb
                    r x =
                      let rv =
                           app ga ( (pair g x) `pair`
                                    (unit `pair`
                                     (pair d (pair x b))))
                      in conj (car rv) (cdr rv)
                in
                app gexists $ lam r

  uc   = sr_comm
  ucE  = sr_commL
  ec   = sr_comm
  ecR  = sr_commL

-- Auxilaries
-- They do not have the general Semantics repr type and so they cannot
-- be used in analyses. They express general structural rules which
-- are not admitted in general.
-- Structural constants instantiate the rules below for specific
-- structural patterns

sr_comm :: (Lam1 l, LambdaPair l) => Sem l ((g,d) :\ a) ((d,g) :\ a)
sr_comm = Sem . lamP $
     \gdFa -> lam $ \dg -> app gdFa (pair (cdr dg) (car dg))


sr_commL :: (Lam1 l, LambdaPair l) =>
            Sem l ((g1,(g2,d)) :\ a) ((g2,(g1,d)) :\ a)
sr_commL = Sem . lamP $
     \g1g2dFa -> lam $ \g2g1d ->
       let g2 = car g2g1d
           g1 = car . cdr $ g2g1d
           d  = cdr . cdr $ g2g1d in
       app g1g2dFa (pair g1 (pair g2 d))


-- additional semantic constants
class LambdaPair l => Lam1 l where
  bill', sue', chicago', detroit', monday', tuesday' :: l Entity
  book', record', gift'         :: l (Entity -> Bool)
  left', tripped', fell', went' :: l (Entity -> Bool)
  see', hate', buy'             :: l (Entity -> Entity -> Bool)
  give' :: l (Entity -> Entity -> Bool)
  to',on'   :: l (Entity -> (Entity -> Bool) -> (Entity -> Bool))
  gexists   :: l ((t -> Bool) -> Bool)
  equal     :: l t -> l t -> l Bool

instance Lam1 l => Constants (Sem l) where
  and' = Sem . lamP $ \_ -> lamP (\y -> lamP (\x -> conj x y))

  john             = Sem . lamP $ \_ -> john'
  mary             = Sem . lamP $ \_ -> mary'
  bill             = Sem . lamP $ \_ -> bill'
  detroit          = Sem . lamP $ \_ -> detroit'
  chicago          = Sem . lamP $ \_ -> chicago'
  monday           = Sem . lamP $ \_ -> monday'
  tuesday          = Sem . lamP $ \_ -> tuesday'
  sue              = Sem . lamP $ \_ -> sue'

  tripped          = Sem . lamP $ \_ -> lamP (app tripped')
  fell             = Sem . lamP $ \_ -> lamP (app fell')
  left             = Sem . lamP $ \_ -> lamP (app left')
  went             = Sem . lamP $ \_ -> lamP (app went')

  book             = Sem . lamP $ \_ -> lamP (app book')
  record           = Sem . lamP $ \_ -> lamP (app record')
  gift             = Sem . lamP $ \_ -> lamP (app gift')

  like             = Sem . lamP $ \_ -> lamP (lamP . (app . (app like')))
  hate             = Sem . lamP $ \_ -> lamP (lamP . (app . (app hate')))
  saw              = Sem . lamP $ \_ -> lamP (lamP . (app . (app see')))
  bought           = Sem . lamP $ \_ -> lamP (lamP . (app . (app buy')))

{-
  to               = Sem to'
  on               = Sem on'
  gave             = Sem $ lam (\o -> lam (\pp -> app pp (app give' o)))
-}

  everyone         = Sem . lamP $ \npo -> car npo
  someone          = Sem . lamP $ \npo -> car npo
  a                = Sem . lamP $ \np  -> lam $ \cn ->
   let cv = app cn np
   in pair np (conj (car cv) (cdr cv))

  qForall          = Sem . lamP $ \_ -> lamP $ \k ->
    app forall (lam $ \x ->
                 let rs = app k (pair x unit) in
                 impl (cdr rs) (car rs))

  qExists          = Sem . lamP $ \_ -> lamP $ \k ->
    app exists (lam $ \x ->
                 let rs = app k (pair x unit) in
                 conj (cdr rs) (car rs))

  -- just return the referent
  ref             = Sem . lamP $ \nnp -> lam $ \np ->
                            pair np (equal np nnp)

-- Pretty-printing

instance LambdaPair C where
  pair (C a) (C b) = C $ \i p -> paren True (a i 1 ++ ", " ++ b i 0)
  car (C x) = C $ \i p -> paren (p>10) ("fst " ++ x i 11)
  cdr (C x) = C $ \i p -> paren (p>10) ("snd " ++ x i 11)
  unit      = C $ \_ _ -> "()"

instance Lam1 C where
  bill'           = C (\_ _ -> "bill'")
  sue'            = C (\_ _ -> "sue'")
  monday'         = C (\_ _ -> "monday'")
  tuesday'        = C (\_ _ -> "tuesday'")
  detroit'        = C (\_ _ -> "detroit'")
  chicago'        = C (\_ _ -> "chicago'")

  book'           = C (\_ _ -> "book'")
  record'         = C (\_ _ -> "record'")
  gift'           = C (\_ _ -> "present'")
  on'             = C (\_ _ -> "on'")
  to'             = C (\_ _ -> "to'")

  tripped'        = C (\_ _ -> "tripped'")
  fell'           = C (\_ _ -> "fell'")
  left'           = C (\_ _ -> "left'")
  went'           = C (\_ _ -> "went'")
  hate'           = C (\_ _ -> "hate'")
  see'            = C (\_ _ -> "see'")
  buy'            = C (\_ _ -> "buy'")
  give'           = C (\_ _ -> "give'")
  gexists         = C (\_ _ -> "E!")
  equal (C x) (C y) = C (\i p -> paren (p > 2) (x i 3 ++ " = " ++ y i 2))


-- Partial evaluation

type instance Known lrepr (a,b) = (P lrepr a, P lrepr b)
type instance Known lrepr ()    = ()

instance LambdaPair l => LambdaPair (P l) where
  pair p1 p2 = P (pair (unP p1) (unP p2)) (Just (p1,p2))
  car (P x Nothing)      = P (car x) Nothing
  car (P _ (Just (x,y))) = x
  cdr (P x Nothing)      = P (cdr x) Nothing
  cdr (P _ (Just (x,y))) = y
  unit                   = P unit (Just ())
  
instance Lam1 l => Lam1 (P l) where
  bill'          = unknown bill'
  sue'           = unknown sue'
  monday'        = unknown monday'
  tuesday'       = unknown tuesday'
  detroit'       = unknown detroit'
  chicago'       = unknown chicago'
  book'          = unknown book'
  record'        = unknown record'
  gift'          = unknown gift'
  on'            = unknown on'
  to'            = unknown to'
  tripped'       = unknown tripped'
  fell'          = unknown fell'
  left'          = unknown left'
  went'          = unknown went'
  hate'          = unknown hate'
  see'           = unknown see'
  buy'           = unknown buy'
  give'          = unknown give'
  gexists        = unknown gexists
  equal (P x _) (P y _) = unknown (equal x y)

shsem :: P C a -> String
shsem = show

